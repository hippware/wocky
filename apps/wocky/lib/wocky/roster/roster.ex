defmodule Wocky.Roster do
  @moduledoc """
  DB interface module for roster items
  """

  import Ecto.Query

  alias Ecto.Queryable
  alias Wocky.{Repo, User}
  alias Wocky.Roster.{Invitation, Item}

  require Logger

  @type relationship :: :self | :friend | :invited | :invited_by | :none
  @type error :: {:error, term()}

  # ----------------------------------------------------------------------
  # Database interaction

  @spec get_item(User.t(), User.t()) :: Item.t() | nil
  def get_item(a, b), do: Item.get(a, b)

  @spec set_name(User.t(), User.t(), binary()) :: {:ok, Item.t()} | error()
  def set_name(user, contact, name), do: Item.set_name(user, contact, name)

  @doc "Returns the relationship of user to target"
  @spec relationship(User.t(), User.t()) :: relationship
  def relationship(%User{id: id}, %User{id: id}), do: :self

  def relationship(user, target) do
    # TODO: This can be optimised into a single query with some JOIN magic,
    # but I'm going for "simple and working" first.
    cond do
      friend?(user, target) -> :friend
      invited?(user, target) -> :invited
      invited_by?(user, target) -> :invited_by
      true -> :none
    end
  end

  @doc "Returns true if the two users are friends"
  @spec friend?(User.t() | User.id(), User.t() | User.id()) :: boolean
  def friend?(%User{id: user_a_id}, %User{id: user_b_id}),
    do: friend?(user_a_id, user_b_id)

  def friend?(user_a_id, user_b_id) do
    Item
    |> where(user_id: ^user_a_id)
    |> where(contact_id: ^user_b_id)
    |> Repo.one()
    |> Kernel.!=(nil)
  end

  @doc "Returns true if the first user has invited the second to be friends"
  @spec invited?(User.t(), User.t()) :: boolean
  def invited?(user, target) do
    Invitation
    |> where(user_id: ^user.id)
    |> where(invitee_id: ^target.id)
    |> Repo.one()
    |> Kernel.!=(nil)
  end

  @doc "Returns true if the roster item refers to a followee of the item owner"
  @spec invited_by?(User.t(), User.t()) :: boolean
  def invited_by?(user, target), do: invited?(target, user)

  @doc """
  Invites `contact` to become a friend of `user`
  """
  @spec invite(User.t(), User.t()) :: :friend | :invited
  def invite(user, target) do
    case relationship(user, target) do
      :none ->
        :ok = Invitation.add(user, target)
        :invited

      :invited_by ->
        befriend(user, target)
        :friend

      :invited ->
        :invited

      :friend ->
        :friend
    end
  end

  @spec befriend(User.t(), User.t()) :: :ok
  def befriend(a, b) do
    {:ok, _} = Item.add(a, b)
    {:ok, _} = Item.add(b, a)
    Invitation.delete_pair(a, b)
    :ok
  end

  @doc "Removes all relationships (friend + follow) between the two users"
  @spec unfriend(User.t(), User.t()) :: :ok
  def unfriend(a, b) do
    Item.delete_pair(a, b)
    Invitation.delete_pair(a, b)
    :ok
  end

  # ----------------------------------------------------------------------
  # Queries

  @spec friends_query(User.t(), User.t()) :: Queryable.t() | error()
  def friends_query(%User{id: id} = user, %User{id: id}),
    do: Item.friends_query(user)

  def friends_query(_, _), do: {:error, :permission_denied}

  @spec sent_invitations_query(User.t(), User.t()) :: Queryable.t() | error()
  def sent_invitations_query(%User{id: id} = user, %User{id: id}),
    do: Invitation.sent_query(user)

  def sent_invitations_query(_, _), do: {:error, :permission_denied}

  @spec received_invitations_query(User.t(), User.t()) ::
          Queryable.t() | error()
  def received_invitations_query(%User{id: id} = user, %User{id: id}),
    do: Invitation.received_query(user)

  def received_invitations_query(_, _), do: {:error, :permission_denied}

  @spec items_query(User.t(), User.t()) :: Queryable.t() | error()
  def items_query(%User{id: id} = user, %User{id: id}),
    do: Item.items_query(user)

  def items_query(_, _), do: {:error, :permission_denied}
end
