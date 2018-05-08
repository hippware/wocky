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

  alias Ecto.Adapters.SQL
  alias Ecto.{Queryable, UUID}
  alias Wocky.Block
  alias Wocky.Repo
  alias Wocky.Roster.{InitialContact, Item}
  alias Wocky.User

  require Logger

  @type version :: binary
  @type relationship :: :self | :friend | :follower | :followee | :none

  @blocked_group "__blocked__"
  @blocked_by_group "__blocked_by__"

  @doc "Write a roster record to the database"
  @spec put(map) :: {:ok, Item.t()} | {:error, term}
  def put(fields) do
    %Item{}
    |> Item.changeset(remove_block_groups(fields))
    |> Repo.insert(
      on_conflict: :replace_all,
      conflict_target: [:user_id, :contact_id]
    )
  end

  @spec get(User.id()) :: [Item.t()]
  def get(user_id) do
    Item
    |> with_user(user_id)
    |> preload(:contact)
    |> Repo.all()
    |> add_blocking_groups(user_id)
  end

  @spec get(User.id(), User.id()) :: Item.t() | nil
  def get(user_id, contact_id) do
    Item
    |> with_contact(contact_id)
    |> preload(:contact)
    |> Repo.get_by(user_id: user_id)
    |> add_blocking_groups(user_id)
  end

  @doc """
  Returns the pair of roster items for a/b if they exist. a's item for
  b will always be the first of the pair
  """
  @spec get_pair(User.id(), User.id()) :: {Item.t(), Item.t()} | nil
  def get_pair(a, b) do
    Item
    |> with_pair(a, b)
    |> Repo.all()
    |> maybe_sort_pair(a, b)
  end

  @spec version(User.id()) :: version
  def version(user_id) do
    timestamp =
      Item
      |> with_user(user_id)
      |> select([r], max(r.updated_at))
      |> Repo.one()

    count =
      Item
      |> with_user(user_id)
      |> count()
      |> Repo.one()

    make_version(timestamp, count)
  end

  @spec delete(User.id()) :: :ok
  def delete(user_id) do
    Item
    |> with_user(user_id)
    |> Repo.delete_all()

    :ok
  end

  @spec delete(User.id(), User.id()) :: :ok
  def delete(user_id, contact_id) do
    Item
    |> with_user(user_id)
    |> with_contact(contact_id)
    |> Repo.delete_all()

    :ok
  end

  @spec find_users_with_contact(User.id()) :: [User.t()]
  def find_users_with_contact(contact_id) do
    Item
    |> with_contact(contact_id)
    |> preload(:user)
    |> Repo.all()
    |> Enum.map(&Map.get(&1, :user))
  end

  @spec has_contact(User.id(), User.id()) :: boolean
  def has_contact(user_id, contact_id) do
    Item
    |> with_user(user_id)
    |> with_contact(contact_id)
    |> where(ask: ^:none)
    |> count()
    |> Repo.one()
    |> Kernel.!=(0)
  end

  @doc "Gets all followers of a user"
  @spec followers(User.id(), boolean) :: [User.t()]
  def followers(user_id, include_system \\ true) do
    user_id
    |> followers_query(user_id, include_system)
    |> Repo.all()
  end

  @doc "Gets all users being followed by the user"
  @spec followees(User.id(), boolean) :: [User.t()]
  def followees(user_id, include_system \\ true) do
    user_id
    |> followees_query(user_id, include_system)
    |> Repo.all()
  end

  @spec friends(User.id(), boolean) :: [User.t()]
  def friends(user_id, include_system \\ true) do
    user_id
    |> friends_query(user_id, include_system)
    |> Repo.all()
  end

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
    |> join(:left, [u], r in Item, u.id == r.contact_id)
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

  @doc """
  Returns true if `user_id` is a friend of `contact_id`, regardless of
  whether contact_id has blocked `user_id`
  """
  @spec is_friend(User.id(), User.id()) :: boolean
  def is_friend(user_id, contact_id) do
    user_id |> get(contact_id) |> is_friend
  end

  def call_stored_relationship_proc(user1, user2, proc) do
    {:ok, u1} = UUID.dump(user1)
    {:ok, u2} = UUID.dump(user2)

    Repo
    |> SQL.query!("SELECT #{proc}($1, $2)", [u1, u2])
    |> Map.get(:rows)
    |> hd
    |> hd
  end

  @doc "Returns true if `user_id` is a follower of contact_id"
  @spec is_follower(User.id(), User.id()) :: boolean
  def is_follower(user_id, contact_id) do
    user_id |> get(contact_id) |> is_followee
  end

  @doc "Returns the relationship of a to b"
  @spec relationship(User.id(), User.id()) :: relationship
  def relationship(a, a), do: :self

  def relationship(a, b) do
    case get_pair(a, b) do
      nil ->
        :none

      {a_to_b, b_to_a} ->
        cond do
          is_friend(a_to_b) ->
            :friend

          is_follower(a_to_b) ->
            :followee

          is_follower(b_to_a) ->
            :follower

          true ->
            :none
        end
    end
  end

  @spec add_initial_contact(User.t(), InitialContact.type()) :: :ok
  def add_initial_contact(user, type), do: InitialContact.put(user, type)

  @spec add_initial_contacts_to_user(User.id()) :: :ok
  def add_initial_contacts_to_user(user_id),
    do: InitialContact.add_to_user(user_id)

  @spec befriend(User.id(), User.id()) :: :ok
  def befriend(u1, u2) do
    add_relationship(u1, u2, :both)
    add_relationship(u2, u1, :both)
    :ok
  end

  @spec follow(User.id(), User.id()) :: :ok
  def follow(follower, followee) do
    add_relationship(followee, follower, :from)
    add_relationship(follower, followee, :to)
    :ok
  end

  # Removes all relationships (friend + follow) between the two users
  @spec unfriend(User.id(), User.id()) :: :ok
  def unfriend(a, b) do
    Item
    |> with_pair(a, b)
    |> Repo.delete_all()

    :ok
  end

  def blocked_group, do: @blocked_group
  def blocked_by_group, do: @blocked_by_group

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

  @spec bump_all_versions(User.id()) :: :ok
  def bump_all_versions(contact_id) do
    Item
    |> with_contact(contact_id)
    |> Repo.update_all(set: [updated_at: NaiveDateTime.utc_now()])

    :ok
  end

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

  defp with_user(query, user_id) do
    from r in query, where: r.user_id == ^user_id
  end

  defp with_contact(query, contact_id) do
    from r in query, where: r.contact_id == ^contact_id
  end

  defp with_pair(query, a, b) do
    from r in query,
      where:
        (r.user_id == ^a and r.contact_id == ^b) or
          (r.user_id == ^b and r.contact_id == ^a)
  end

  defp count(query) do
    from r in query, select: count(r.contact_id)
  end

  defp make_version(nil, count) do
    normalise_version("0", count)
  end

  defp make_version(ver, count) do
    ver
    |> Timex.to_gregorian_microseconds()
    |> Integer.to_string()
    |> normalise_version(count)
  end

  defp normalise_version(verstr, count) do
    verstr <> "-" <> Integer.to_string(count)
  end

  defp is_friend(%Item{subscription: :both}), do: true
  defp is_friend(_), do: false

  # Returns true if the roster item referrs to a follower of the item owner
  defp is_follower(%Item{subscription: subscription}) do
    subscription == :both || subscription == :from
  end

  # Returns true if the roster item referrs to a followee of the item owner
  defp is_followee(%Item{subscription: subscription}) do
    subscription == :both || subscription == :to
  end

  defp is_followee(_), do: false

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

  defp add_blocking_groups(nil, _), do: nil

  defp add_blocking_groups(%Item{} = item, user_id) do
    [item] |> add_blocking_groups(user_id) |> hd()
  end

  defp add_blocking_groups(items, user_id) do
    blocks = Block.blocks(user_id)

    Enum.map(items, &add_blocking_groups_to_item(&1, blocks))
  end

  defp add_blocking_groups_to_item(item, blocks) do
    item
    |> add_blocked_group(blocks)
    |> add_blocked_by_group(blocks)
  end

  defp add_blocked_group(
    %Item{user_id: user_id, contact_id: contact_id, groups: groups} = item,
    blocks) do
    case Enum.any?(blocks, &is_block(user_id, contact_id, &1)) do
      true -> %{item | groups: [@blocked_group | groups]}
      false -> item
    end
  end

  defp add_blocked_by_group(
    %Item{user_id: user_id, contact_id: contact_id, groups: groups} = item,
    blocks) do
    case Enum.any?(blocks, &is_block(contact_id, user_id, &1)) do
      true -> %{item | groups: [@blocked_by_group | groups]}
      false -> item
    end
  end

  defp is_block(blocker_id, blockee_id, block),
  do: block.blocker_id == blocker_id && block.blockee_id == blockee_id

  defp remove_block_groups(%{groups: groups} = fields) do
    %{fields | groups: groups -- [@blocked_group, @blocked_by_group]}
  end

  defp remove_block_groups(fields), do: fields
end
