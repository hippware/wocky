defmodule Wocky.Roster do
  @moduledoc """
  DB interface module for roster items

  NOTE (Because I'm forever messing this up):

  Roster item subscriptions should be read in the following way:

  [contact_id] sends/gets presences [suscription] [user_id]

  eg:
  %Item{
    user_id: A
    contact_id: B
    subscription: :from
  }

  reads as: "B gets presences from A" (ie B is a follower of A).

  The corresponding entry for B:

  %Item{
    user_id: B
    contact_id: A
    subscription: :to
  }

  reads as: "A sends presences to B" (ie A is a followee of B).

  """

  import Ecto.Query

  alias Ecto.Queryable
  alias Wocky.{Block, Repo}
  alias Wocky.Roster.{InitialContact, Item}
  alias Wocky.User

  require Logger

  @type relationship :: :self | :friend | :follower | :followee | :none

  # ----------------------------------------------------------------------
  # Database interaction

  @doc "Write a roster record to the database"
  @spec put(map) :: {:ok, Item.t()} | {:error, term}
  def put(fields) do
    %Item{}
    |> Item.changeset(fields)
    |> Repo.insert(
      on_conflict: :replace_all,
      conflict_target: [:user_id, :contact_id]
    )
  end

  @doc "Get the roster item for a given user pertaining to another user"
  @spec get(User.id(), User.id()) :: Item.t() | nil
  def get(user_id, contact_id) do
    Item
    |> where(contact_id: ^contact_id)
    |> where(user_id: ^user_id)
    |> preload(:contact)
    |> Repo.one()
  end

  @spec relationship(Item.t()) :: relationship
  def relationship(item) do
    cond do
      friend?(item) -> :friend
      follower?(item) -> :follower
      followee?(item) -> :followee
      true -> :none
    end
  end

  @doc "Returns true if the roster item refers to a friend of the item owner"
  @spec friend?(Item.t()) :: boolean
  def friend?(%Item{subscription: :both}), do: true
  def friend?(_), do: false

  @doc "Returns true if the roster item refers to a follower of the item owner"
  @spec follower?(Item.t()) :: boolean
  def follower?(%Item{subscription: subscription}) do
    subscription == :both || subscription == :from
  end

  def follower?(_), do: false

  @doc "Returns true if the roster item refers to a followee of the item owner"
  @spec followee?(Item.t()) :: boolean
  def followee?(%Item{subscription: subscription}) do
    subscription == :both || subscription == :to
  end

  def followee?(_), do: false

  @doc "Returns the relationship of a to b"
  @spec relationship(User.id(), User.id()) :: relationship
  def relationship(a, a), do: :self

  def relationship(a, b) do
    case get_pair(a, b) do
      nil ->
        :none

      {a_to_b, b_to_a} ->
        cond do
          friend?(a_to_b) ->
            :friend

          follower?(a_to_b) ->
            :followee

          follower?(b_to_a) ->
            :follower

          true ->
            :none
        end
    end
  end

  defp get_pair(a, b) do
    Item
    |> with_pair(a, b)
    |> Repo.all()
    |> maybe_sort_pair(a, b)
  end

  defp with_pair(query, a, b) do
    from r in query,
      where:
        (r.user_id == ^a and r.contact_id == ^b) or
          (r.user_id == ^b and r.contact_id == ^a)
  end

  defp maybe_sort_pair([], _, _), do: nil
  defp maybe_sort_pair([_], _, _), do: nil

  defp maybe_sort_pair([first = %Item{user_id: id}, second], id, _) do
    {first, second}
  end

  defp maybe_sort_pair([first, second], _, _) do
    {second, first}
  end

  defp maybe_sort_pair(list, a, b) do
    :ok =
      Logger.warn(
        "Expected a roster pair but got #{inspect(list)} for #{a}, #{b}"
      )

    nil
  end

  @doc """
  Makes a user a follower of another user but does not remove them as a
  friend, nor the other user as a follower if they are already.
  """
  @spec become_follower(User.id(), User.id()) :: relationship()
  def become_follower(user_id, contact_id) do
    case relationship(user_id, contact_id) do
      :none ->
        :ok = follow(user_id, contact_id)
        :follower

      :followee ->
        :ok = befriend(user_id, contact_id)
        :friend

      :follower ->
        :follower

      :friend ->
        :friend
    end
  end

  @doc """
  Removes a user as a follower of another user but does not change the other
  user's following state of the first user.
  """
  @spec stop_following(User.id(), User.id()) :: relationship()
  def stop_following(user_id, contact_id) do
    case relationship(user_id, contact_id) do
      :none ->
        :none

      :followee ->
        :followee

      :follower ->
        :ok = unfriend(user_id, contact_id)
        :none

      :friend ->
        :ok = follow(contact_id, user_id)
        :followee
    end
  end

  @spec befriend(User.id(), User.id()) :: :ok
  def befriend(u1, u2) do
    {:ok, _} = add_relationship(u1, u2, :both)
    {:ok, _} = add_relationship(u2, u1, :both)
    :ok
  end

  @spec follow(User.id(), User.id()) :: :ok
  def follow(follower, followee) do
    {:ok, _} = add_relationship(followee, follower, :from)
    {:ok, _} = add_relationship(follower, followee, :to)
    :ok
  end

  @doc "Removes all relationships (friend + follow) between the two users"
  @spec unfriend(User.id(), User.id()) :: :ok
  def unfriend(a, b) do
    Item
    |> with_pair(a, b)
    |> Repo.delete_all()

    :ok
  end

  defp add_relationship(user_id, contact_id, subscription) do
    %Item{}
    |> Item.changeset(%{
      user_id: user_id,
      contact_id: contact_id,
      subscription: subscription,
      ask: :none,
      groups: []
    })
    |> Repo.insert(
      on_conflict: [set: [subscription: subscription, ask: :none]],
      conflict_target: [:user_id, :contact_id]
    )
  end

  @spec add_initial_contact(User.t(), InitialContact.type()) :: :ok
  def add_initial_contact(user, type), do: InitialContact.put(user, type)

  @spec add_initial_contacts_to_user(User.id()) :: :ok
  def add_initial_contacts_to_user(user_id),
    do: InitialContact.add_to_user(user_id)

  def write_blocked_items(a, b) do
    [
      %{
        user_id: a.id,
        contact_id: b.id,
        name: "",
        ask: :none,
        subscription: :none,
        groups: []
      },
      %{
        user_id: b.id,
        contact_id: a.id,
        name: "",
        ask: :none,
        subscription: :none,
        groups: []
      }
    ]
    |> Enum.each(&put/1)
  end

  # ----------------------------------------------------------------------
  # Queries

  @spec all_contacts_query(User.id(), User.id(), boolean) :: Queryable.t()
  def all_contacts_query(user_id, requester_id, include_system \\ true) do
    relationships_query(
      user_id,
      requester_id,
      [:both, :from, :to],
      include_system
    )
  end

  @spec followers_query(User.id(), User.id(), boolean) :: Queryable.t()
  def followers_query(user_id, requester_id, include_system \\ true) do
    relationships_query(user_id, requester_id, [:both, :from], include_system)
  end

  @spec followees_query(User.id(), User.id(), boolean) :: Queryable.t()
  def followees_query(user_id, requester_id, include_system \\ true) do
    relationships_query(user_id, requester_id, [:both, :to], include_system)
  end

  @spec friends_query(User.id(), User.id(), boolean) :: Queryable.t()
  def friends_query(user_id, requester_id, include_system \\ true) do
    relationships_query(user_id, requester_id, [:both], include_system)
  end

  defp relationships_query(user_id, requester_id, sub_types, include_system) do
    User
    |> join(:left, [u], r in Item, on: u.id == r.contact_id)
    |> where([u, r], r.user_id == ^user_id)
    |> maybe_filter_system(not include_system)
    |> where([u, r], not is_nil(u.handle))
    |> where([u, r], r.subscription in ^sub_types)
    |> Block.object_visible_query(requester_id, :contact_id)
  end

  defp maybe_filter_system(query, false), do: query

  defp maybe_filter_system(query, true) do
    query
    |> where([u, r], ^User.system_role() not in u.roles)
  end
end
