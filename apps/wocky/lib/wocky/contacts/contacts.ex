defmodule Wocky.Contacts do
  @moduledoc """
  Context module for managing a user's relationships with others.
  """

  use Elixometer
  use Wocky.Context

  alias Ecto.Multi
  alias Wocky.Account.User
  alias Wocky.Contacts.Relationship
  alias Wocky.Contacts.Share
  alias Wocky.Contacts.Share.Cache
  alias Wocky.Contacts.Share.CachedRelationship
  alias Wocky.Events.UserInvitation
  alias Wocky.Events.UserInvitationResponse
  alias Wocky.Notifier

  require Logger

  @type error :: {:error, any()}
  @type share_type :: Relationship.share_type()
  @type relationship :: Relationship.state() | :none
  @type relationship_state ::
          Relationship.state() | :invited_by | :blocked_by | :self | :none
  @type relationship_details ::
          {relationship_state(), Relationship.t() | nil, Relationship.t() | nil}

  @upsert_opts [
    on_conflict: {:replace, [:share_type, :state]},
    conflict_target: [:user_id, :contact_id]
  ]

  defdelegate share_types, to: Relationship

  # ----------------------------------------------------------------------
  # Relationship management

  @doc """
  Befriends two users.

  NOTE The sharing level will be the same for both users. The sharing level
  can be passed in the `opts` parameter with the key `:share_type`. If no
  share type is passed, sharing defaults to `:disabled`.

  To befriend two users with different sharing types, use `make_friends/3`.
  """
  @spec befriend(User.t(), User.t(), atom()) :: :ok | Repo.error()
  def befriend(user, contact, share_type \\ :disabled) do
    with {:ok, _} <- friend_upsert({user, share_type}, {contact, share_type}) do
      :ok
    end
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
  @spec make_friends(User.t(), User.tid(), share_type()) ::
          {:ok, :friend | :invited} | Repo.error()
  def make_friends(user, contact, share_type) do
    case get_relationship(user, contact) do
      {:none, _, _} ->
        invite_user(user, contact, share_type, true)

      {:invited, _, _} ->
        invite_user(user, contact, share_type, false)

      {:invited_by, _, rel} ->
        do_make_friends(
          {user, share_type},
          {User.hydrate(contact), rel.share_type}
        )

      {:friend, _, _} ->
        {:ok, :friend}

      {:self, _, _} ->
        make_error(user, contact, %{share_type: share_type}, "self")

      {state, _, _} when state in [:blocked, :blocked_by] ->
        make_error(user, contact, %{share_type: share_type}, "blocked")
    end
  end

  defp invite_user(user, contact, share_type, notify) do
    with {:ok, _} <- do_invite_user(user, contact, share_type) do
      if notify do
        %UserInvitation{
          from: user,
          to: User.hydrate(contact)
        }
        |> Notifier.notify()
      end

      {:ok, :invited}
    end
  end

  defp do_invite_user(user, contact, share_type) do
    user
    |> Relationship.upsert_changeset(contact, :invited, share_type)
    |> Repo.insert(@upsert_opts)
  end

  defp do_make_friends({user, _} = u_opts, {contact, _} = c_opts) do
    with {:ok, _} <- friend_upsert(u_opts, c_opts) do
      %UserInvitationResponse{
        from: user,
        to: contact
      }
      |> Notifier.notify()

      {:ok, :friend}
    end
  end

  defp friend_upsert({user, user_sharing}, {contact, contact_sharing}) do
    upsert_both_users(
      {user, :friend, user_sharing},
      {contact, :friend, contact_sharing}
    )
  end

  defp upsert_both_users(
         {user, user_state, user_sharing},
         {contact, contact_state, contact_sharing}
       ) do
    Multi.new()
    |> multi_upsert(:user, user, contact, user_state, user_sharing)
    |> multi_upsert(:contact, contact, user, contact_state, contact_sharing)
    |> Repo.transaction()
  end

  defp multi_upsert(multi, name, user, contact, state, sharing) do
    changeset = Relationship.upsert_changeset(user, contact, state, sharing)

    Multi.insert(multi, name, changeset, @upsert_opts)
  end

  @doc """
  Removes all relationships (invitation + friend) between the two users.

  Does NOT remove blocks.
  """
  @spec unfriend(User.tid(), User.tid()) :: :ok
  def unfriend(user, contact) do
    # NOTE there are triggers that clean up bot invitations and
    # subscriptions between the two users.
    Repo.delete_all(nonblocked_pair(user, contact))

    :ok
  end

  @doc "User initiates a block on a contact"
  @spec block(User.t(), User.tid()) :: :ok | Repo.error()
  def block(user, contact) do
    # NOTE there are triggers that clean up bot invitations and
    # subscriptions as well as notifications between the two users.
    case do_block(user, contact) do
      {:ok, _} ->
        update_counter("blocking.blocked", 1)

        :ok

      {:error, _, cs, _} ->
        {:error, cs}

      {:error, _} = error ->
        error
    end
  end

  defp do_block(user, contact) do
    Multi.new()
    |> multi_upsert(:user, user, contact, :blocked, :disabled)
    |> Multi.delete_all(:cleanup, nonblocked_pair(user, contact))
    |> Repo.transaction()
  end

  @spec unblock(User.tid(), User.tid()) :: :ok
  def unblock(user, contact) do
    Repo.delete_all(
      from r in Relationship,
        where: r.user_id == ^User.id(user),
        where: r.contact_id == ^User.id(contact),
        where: r.state == ^:blocked
    )

    update_counter("blocking.unblocked", 1)

    :ok
  end

  @spec update_last_start_notification(User.t(), User.tid()) ::
          Repo.result(Relationship.t())
  def update_last_start_notification(user, contact) do
    update_relationship(user, contact, %{
      nearby_last_start_notification: DateTime.utc_now()
    })
  end

  @spec update_sharing(User.t(), User.tid(), share_type(), Keyword.t()) ::
          Repo.result(Relationship.t())
  def update_sharing(user, contact, share_type, opts \\ []) do
    params =
      opts
      |> Enum.into(%{})
      |> Map.take([:nearby_distance, :nearby_cooldown])
      |> Map.put(:share_type, share_type)

    update_relationship(user, contact, params)
  end

  defp update_relationship(user, contact, changes) do
    case Repo.one(single_relationship(user, contact)) do
      nil ->
        make_error(user, contact, changes)

      relationship ->
        update_relationship(relationship, changes)
    end
  end

  defp update_relationship(relationship, changes) do
    relationship
    |> Relationship.changeset(changes)
    |> Repo.update()
  end

  defp make_error(user, contact, changes, message \\ "must be a friend") do
    {:error, Relationship.error_changeset(user, contact, changes, message)}
  end

  @doc "Stops location sharing with all friends"
  @spec stop_sharing_location(User.t()) :: :ok
  def stop_sharing_location(user) do
    Repo.update_all(
      assoc(user, :relationships),
      set: [
        share_type: :disabled,
        share_changed_at: DateTime.utc_now(),
        updated_at: DateTime.utc_now()
      ]
    )

    :ok
  end

  # ----------------------------------------------------------------------
  # Relationship status

  @spec get_contact_user(Relationship.t()) :: User.t()
  def get_contact_user(%Relationship{} = r) do
    Repo.preload(r, [:contact]).contact
  end

  @spec relationship(User.tid(), User.tid()) :: relationship()
  def relationship(user, contact) do
    {state, _, _} = get_relationship(user, contact)
    state
  end

  @spec share_type(User.t(), User.tid()) :: share_type()
  def share_type(user, contact) do
    case do_share_type(user, contact) do
      nil -> :disabled
      type -> type
    end
  end

  defp do_share_type(user, contact) do
    Repo.one(
      from r in assoc(user, :relationships),
        where: r.contact_id == ^User.id(contact),
        select: r.share_type
    )
  end

  @spec get_relationship(User.tid(), User.tid()) ::
          relationship_details() | Repo.error()
  def get_relationship(user, contact) do
    if self?(user, contact) do
      {:self, nil, nil}
    else
      do_get_relationship(user, contact)
    end
  end

  defp do_get_relationship(user, contact) do
    user_id = User.id(user)
    contact_id = User.id(contact)

    case Repo.all(relationship_pair(user, contact)) do
      [] ->
        {:none, nil, nil}

      [%Relationship{user_id: ^user_id} = rel] ->
        {rel.state, rel, nil}

      [%Relationship{user_id: ^contact_id} = rel] ->
        rel_state =
          case rel.state do
            :invited -> :invited_by
            :blocked -> :blocked_by
          end

        {rel_state, nil, rel}

      [%Relationship{state: state}, %Relationship{state: state}] = rels ->
        user_rel = Enum.find(rels, fn %{user_id: id} -> id == user_id end)
        contact_rel = Enum.find(rels, fn %{user_id: id} -> id == contact_id end)

        {state, user_rel, contact_rel}

      {:error, _} = error ->
        error
    end
  end

  @spec self?(User.tid(), User.tid()) :: boolean()
  def self?(user, contact), do: User.id(user) == User.id(contact)

  @doc "Returns true if the two users are friends"
  @spec friend?(User.t(), User.tid()) :: boolean
  def friend?(user, contact) do
    Repo.exists?(relationship_with_state(user, contact, :friend))
  end

  @doc "Returns true if the first user has invited the second to be friends"
  @spec invited?(User.tid(), User.tid()) :: boolean
  def invited?(user, contact) do
    Repo.exists?(relationship_with_state(user, contact, :invited))
  end

  @doc "Returns true if the first user has been invited by the second to be friends"
  @spec invited_by?(User.t(), User.t()) :: boolean
  def invited_by?(user, contact) do
    invited?(contact, user)
  end

  @spec blocked?(User.tid(), User.tid()) :: boolean()
  def blocked?(user, contact) do
    Repo.exists?(blocked_pair(user, contact))
  end

  @spec refresh_share_cache(User.id()) :: [CachedRelationship.t()]
  def refresh_share_cache(user_id), do: Cache.refresh(user_id)

  @spec have_shares([User.id()]) :: [User.id()]
  def have_shares(user_ids) do
    Repo.all(
      from r in Relationship,
        where: r.user_id in ^user_ids and r.share_type != ^:disabled,
        select: r.user_id,
        distinct: true
    )
  end

  # ----------------------------------------------------------------------
  # Queries

  defp single_relationship(user, contact) do
    from r in assoc(user, :relationships),
      where: r.contact_id == ^User.id(contact)
  end

  defp relationship_pair(user, contact) do
    user_id = User.id(user)
    contact_id = User.id(contact)

    from r in Relationship,
      where:
        (r.user_id == ^user_id and r.contact_id == ^contact_id) or
          (r.user_id == ^contact_id and r.contact_id == ^user_id)
  end

  defp nonblocked_pair(user, contact) do
    from r in relationship_pair(user, contact),
      where: r.state != ^:blocked
  end

  defp blocked_pair(user, contact) do
    from r in relationship_pair(user, contact),
      where: r.state == ^:blocked
  end

  defp relationship_with_state(user, contact, state) do
    from r in single_relationship(user, contact),
      where: r.state == ^state
  end

  defp with_same_user(user, requestor, fun) do
    if User.id(user) == User.id(requestor) do
      fun.(user)
    else
      {:error, :permission_denied}
    end
  end

  @spec sent_invitations_query(User.t(), User.t()) :: Queryable.t() | error()
  def sent_invitations_query(user, requestor) do
    with_same_user(user, requestor, fn user ->
      from r in assoc(user, :relationships),
        where: r.state == ^:invited
    end)
  end

  @spec received_invitations_query(User.t(), User.t()) ::
          Queryable.t() | error()
  def received_invitations_query(user, requestor) do
    with_same_user(user, requestor, fn user ->
      from r in Relationship,
        where: r.contact_id == ^User.id(user),
        where: r.state == ^:invited
    end)
  end

  @spec friends_query(User.tid(), User.tid()) :: Queryable.t() | error()
  def friends_query(user, requestor) do
    with_same_user(user, requestor, fn user ->
      from u in User,
        left_join: r in Relationship,
        on: u.id == r.contact_id,
        where: r.user_id == ^User.id(user) and r.state == ^:friend
    end)
  end

  @spec friend_relationships_query(User.t(), User.t()) ::
          Queryable.t() | error()
  def friend_relationships_query(user, requestor) do
    with_same_user(user, requestor, fn user ->
      from r in assoc(user, :relationships),
        where: r.state == ^:friend
    end)
  end

  @spec get_location_share_targets(User.tid()) :: [CachedRelationship.t()]
  def get_location_share_targets(user), do: Cache.get(user)

  @spec get_location_shares(User.tid()) :: [Share.t()]
  def get_location_shares(user) do
    user
    |> get_location_shares_query()
    |> Repo.all()
  end

  @spec get_location_shares_query(User.tid()) :: Queryable.t()
  def get_location_shares_query(user) do
    from s in location_shares_query(),
      where: s.user_id == ^User.id(user)
  end

  @spec get_location_sharers(User.tid()) :: [Share.t()]
  def get_location_sharers(user) do
    user
    |> get_location_sharers_query()
    |> Repo.all()
  end

  @spec get_location_sharers_query(User.tid()) :: Queryable.t()
  def get_location_sharers_query(user) do
    from s in location_shares_query(),
      where: s.contact_id == ^User.id(user)
  end

  defp location_shares_query do
    from r in Relationship,
      where: r.share_type != ^:disabled,
      order_by: [desc: r.share_changed_at],
      preload: [:user, :contact]
  end

  @spec blocks_query(User.t()) :: Queryable.t()
  def blocks_query(user) do
    from r in assoc(user, :relationships),
      where: r.state == ^:blocked,
      preload: :contact
  end

  @doc """
  Composable query fragment to filter out objects with owners that are blocking/
  blocked by the supplied user.
  """
  @spec object_visible_query(Queryable.t(), User.tid(), atom()) :: Queryable.t()
  def object_visible_query(query, requester, owner_field \\ :user_id) do
    from o in query,
      left_join: r in Relationship,
      on:
        ((field(o, ^owner_field) == r.user_id and
            r.contact_id == ^User.id(requester)) or
           (field(o, ^owner_field) == r.contact_id and
              r.user_id == ^User.id(requester))) and r.state == ^:blocked,
      where: is_nil(r.user_id)
  end
end
