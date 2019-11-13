defmodule Wocky.Friends do
  @moduledoc """
  Context module for managing the friends list.
  """

  import Ecto.Query

  alias Ecto.Changeset
  alias Ecto.Queryable
  alias Wocky.Account.User
  alias Wocky.Block
  alias Wocky.Events.UserInvitationResponse
  alias Wocky.Friends.Friend
  alias Wocky.Friends.Invitation
  alias Wocky.Friends.Share
  alias Wocky.Friends.Share.Cache
  alias Wocky.Friends.Share.CachedFriend
  alias Wocky.Notifier
  alias Wocky.Repo

  require Logger

  @type share_type :: Friend.share_type()
  @type relationship :: :self | :friend | :invited | :invited_by | :none
  @type error :: {:error, term()}

  @spec get_friend(User.tid(), User.tid()) :: Friend.t() | nil
  def get_friend(user, friend), do: Friend.get(user, friend)

  @doc """
  Befriends two users.

  NOTE The sharing level will be the same for both users. The sharing level
  can be passed in the `opts` parameter with the key `:share_type`. If no
  share type is passed, sharing defaults to `:disabled`.

  To befriend two users with different sharing types, use `make_friends/3`.
  """
  @spec befriend(User.t(), User.t(), Keyword.t()) ::
          :ok | {:error, Changeset.t()}
  def befriend(user, contact, opts \\ []) do
    notify = Keyword.get(opts, :notify, true)
    share_type = Keyword.get(opts, :share_type, :disabled)

    do_make_friends({user, share_type}, {contact, share_type}, notify)
  end

  @doc """
  Progresses two users towards becoming friends

  * If the users are strangers, invites `contact` to become a friend of `user`
  * If an invitation from `user` to `contact` exists, the share type is updated
  * If `contact` was invited by `user`, the two become friends
  * If the users are already friends, nothing is changed

  To update the share type once users are friends, use `update_sharing/4` or
  `stop_sharing_location/1`.
  """
  @spec make_friends(User.tid(), User.tid(), share_type()) ::
          {:ok, :friend | :invited | :self} | Repo.error()
  def make_friends(user, contact, share_type) do
    case get_relationship(user, contact) do
      {:none, _} ->
        do_invite(user, contact, share_type)

      {:invited, _invitation} ->
        do_invite(user, contact, share_type)

      {:invited_by, invitation} ->
        do_make_friends(invitation, share_type)

      {:friend, _} ->
        {:ok, :friend}

      {:self, _} ->
        make_error(user, contact, share_type, "self")

      {:blocked, _} ->
        make_error(user, contact, share_type, "blocked")
    end
  end

  defp make_error(user, contact, share_type, message) do
    {:error,
     Invitation.make_error(user, contact, share_type, :invitee_id, message)}
  end

  defp do_invite(user, contact, share_type) do
    case Invitation.add(user, contact, share_type) do
      {:ok, _} -> {:ok, :invited}
      {:error, _} = error -> error
    end
  end

  defp do_make_friends(invitation, share_type) do
    invitation = Repo.preload(invitation, [:user, :invitee])

    result =
      do_make_friends(
        {invitation.user, invitation.share_type},
        {invitation.invitee, share_type},
        true
      )

    case result do
      :ok -> {:ok, :friend}
      {:error, _} = error -> error
    end
  end

  defp do_make_friends({user, user_sharing}, {contact, contact_sharing}, notify) do
    # TODO These three db operations should happen in a transaction
    with {:ok, _} <- insert_item(user, contact, user_sharing),
         {:ok, _} <- insert_item(contact, user, contact_sharing) do
      Invitation.delete_pair(user, contact)

      if notify do
        %UserInvitationResponse{
          from: user,
          to: contact
        }
        |> Notifier.notify()
      end

      :ok
    end
  end

  defp insert_item(user, contact, share_type) do
    %{
      user_id: User.id(user),
      contact_id: User.id(contact),
      share_type: share_type
    }
    |> Friend.insert_changeset()
    |> Repo.insert(
      on_conflict: {:replace, [:share_type]},
      conflict_target: [:user_id, :contact_id]
    )
  end

  @doc "Removes all relationships (friend + follow) between the two users"
  @spec unfriend(User.tid(), User.tid()) :: :ok
  def unfriend(user, friend) do
    Friend
    |> with_pair(user, friend)
    |> Repo.delete_all()

    Invitation.delete_pair(user, friend)

    :ok
  end

  defp with_pair(query, a, b) do
    a_id = User.id(a)
    b_id = User.id(b)

    from r in query,
      where:
        (r.user_id == ^a_id and r.contact_id == ^b_id) or
          (r.user_id == ^b_id and r.contact_id == ^a_id)
  end

  @spec update_name(User.tid(), User.tid(), String.t()) ::
          Repo.result(Friend.t())
  def update_name(user, friend, name) do
    do_update_item(user, friend, %{name: name})
  end

  @spec update_sharing(User.tid(), User.tid(), share_type(), Keyword.t()) ::
          Repo.result(Friend.t())
  def update_sharing(user, friend, share_type, opts \\ []) do
    params =
      opts
      |> Enum.into(%{})
      |> Map.take([:nearby_distance, :nearby_cooldown])
      |> Map.put(:share_type, share_type)

    do_update_item(user, friend, params)
  end

  @spec update_last_start_notification(User.tid(), User.tid()) ::
          Repo.result(Friend.t())
  def update_last_start_notification(user, friend) do
    {User.id(user), User.id(friend)}
    |> Friend.update_changeset(%{
      nearby_last_start_notification: DateTime.utc_now()
    })
    |> Repo.update()
  end

  defp do_update_item(user, friend, changes) do
    {User.id(user), User.id(friend)}
    |> Friend.update_changeset(changes)
    |> Repo.update()
  end

  @doc "Stops location sharing with all friends"
  @spec stop_sharing_location(User.tid()) :: :ok
  def stop_sharing_location(user) do
    user_id = User.id(user)

    Friend
    |> where(user_id: ^user_id)
    |> Repo.update_all(
      set: [
        share_type: :disabled,
        share_changed_at: DateTime.utc_now(),
        updated_at: DateTime.utc_now()
      ]
    )

    :ok
  end

  @spec get_relationship(User.tid(), User.tid()) :: {atom(), struct() | nil}
  def get_relationship(user, contact) do
    cond do
      User.id(user) == User.id(contact) -> {:self, nil}
      item = Friend.get(user, contact) -> {:friend, item}
      invitation = Invitation.get(user, contact) -> {:invited, invitation}
      invitation = Invitation.get(contact, user) -> {:invited_by, invitation}
      Block.blocked?(User.id(user), User.id(contact)) -> {:blocked, nil}
      true -> {:none, nil}
    end
  end

  @doc "Returns the relationship of user to target"
  @spec relationship(User.tid(), User.tid()) :: relationship()
  def relationship(user, contact) do
    case get_relationship(user, contact) do
      {:blocked, _} -> {:none, nil}
      {relationship, _} -> relationship
    end
  end

  @doc "Returns true if the two users are friends or the same person"
  @spec self_or_friend?(User.tid(), User.tid()) :: boolean
  def self_or_friend?(%User{id: id}, %User{id: id}), do: true
  def self_or_friend?(user_id, user_id), do: true
  def self_or_friend?(a, b), do: friend?(a, b)

  @doc "Returns true if the two users are friends"
  @spec friend?(User.tid(), User.tid()) :: boolean
  def friend?(user, contact), do: !is_nil(Friend.get(user, contact))

  @doc "Returns true if the first user has invited the second to be friends"
  @spec invited?(User.tid(), User.tid()) :: boolean
  def invited?(user, contact), do: !is_nil(Invitation.get(user, contact))

  @doc "Returns true if the first user has been invited by the second to be friends"
  @spec invited_by?(User.t(), User.t()) :: boolean
  def invited_by?(user, target), do: invited?(target, user)

  @spec refresh_share_cache(User.id()) :: [CachedFriend.t()]
  def refresh_share_cache(user_id), do: Cache.refresh(user_id)

  # ----------------------------------------------------------------------
  # Queries

  defp with_same_user(user, requestor, fun) do
    user_id = User.id(user)

    if user_id == User.id(requestor) do
      fun.(user_id)
    else
      {:error, :permission_denied}
    end
  end

  @spec friend_entries_query(User.tid(), User.tid()) :: Queryable.t() | error()
  def friend_entries_query(user, requestor) do
    with_same_user(user, requestor, fn user_id ->
      where(Friend, [i], i.user_id == ^user_id)
    end)
  end

  @spec sent_invitations_query(User.tid(), User.tid()) ::
          Queryable.t() | error()
  def sent_invitations_query(user, requestor) do
    with_same_user(user, requestor, fn user_id ->
      where(Invitation, [r], r.user_id == ^user_id)
    end)
  end

  @spec received_invitations_query(User.tid(), User.tid()) ::
          Queryable.t() | error()
  def received_invitations_query(user, requestor) do
    with_same_user(user, requestor, fn user_id ->
      where(Invitation, [r], r.invitee_id == ^user_id)
    end)
  end

  @spec friends_query(User.tid(), User.tid()) :: Queryable.t() | error()
  def friends_query(user, requestor) do
    with_same_user(user, requestor, fn user_id ->
      User
      |> join(:left, [u], i in Friend, on: u.id == i.contact_id)
      |> where([..., i], i.user_id == ^user_id)
    end)
  end

  @spec get_location_share_targets(User.tid()) :: [CachedFriend.t()]
  def get_location_share_targets(user), do: Cache.get(User.id(user))

  @spec get_location_shares(User.tid()) :: [Share.t()]
  def get_location_shares(user) do
    user
    |> get_location_shares_query()
    |> Repo.all()
  end

  @spec get_location_shares_query(User.tid()) :: Queryable.t()
  def get_location_shares_query(user) do
    user_id = User.id(user)

    location_shares_query()
    |> where([i], i.user_id == ^user_id)
  end

  @spec get_location_sharers(User.tid()) :: [Share.t()]
  def get_location_sharers(user) do
    user
    |> get_location_sharers_query()
    |> Repo.all()
  end

  @spec get_location_sharers_query(User.tid()) :: Queryable.t()
  def get_location_sharers_query(user) do
    user_id = User.id(user)

    location_shares_query()
    |> where([i], i.contact_id == ^user_id)
  end

  defp location_shares_query do
    Friend
    |> preload([:user, :contact])
    |> where([i], i.share_type != "disabled")
    |> order_by([i], desc: i.share_changed_at)
  end
end
