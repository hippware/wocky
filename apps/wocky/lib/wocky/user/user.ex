defmodule Wocky.User do
  @moduledoc "Schema and API for working with users."

  require Logger

  use Wocky.JID
  use Wocky.Repo.Schema

  import Ecto.Query

  alias Ecto.Queryable
  alias Wocky.Account.Token, as: AuthToken
  alias Wocky.Block
  alias Wocky.Bot
  alias Wocky.Bot.{Invitation, Subscription}
  alias Wocky.Conversation
  alias Wocky.Email
  alias Wocky.GeoUtils
  alias Wocky.Message
  alias Wocky.Push.Token, as: PushToken
  alias Wocky.Repo
  alias Wocky.Roster
  alias Wocky.Roster.Item, as: RosterItem
  alias Wocky.TROS.Metadata, as: TROSMetadata
  alias Wocky.User.{Avatar, BotEvent, GeoFence, InviteCode, Location}

  @forever "2200-01-01T00:00:00.000000Z" |> DateTime.from_iso8601() |> elem(1)

  @primary_key {:id, :binary_id, autogenerate: false}
  schema "users" do
    # User ID (userpart of JID)
    field :username, :string
    field :resource, :string, virtual: true
    # The external auth provider
    field :provider, :string
    # The user ID received from the provider
    field :external_id, :string
    # User handle (as seen by other users)
    field :handle, :string
    # ID of file containing user's avatar
    field :avatar, :string
    # User's first name
    field :first_name, :string
    # User's last name
    field :last_name, :string
    # The user's phone number
    field :phone_number, :string
    # (also from auth provider)
    # User's email address
    field :email, :string
    # User's tagline
    field :tagline, :string
    # Password hash
    field :password, :string
    field :pass_details, :string
    field :roles, {:array, :string}, default: []
    field :welcome_sent, :boolean
    field :hidden_until, :utc_datetime

    timestamps()

    has_many :bots, Bot
    has_many :bot_events, BotEvent
    has_many :conversations, Conversation
    has_many :locations, Location
    has_many :push_tokens, PushToken
    has_many :roster_contacts, RosterItem, foreign_key: :contact_id
    has_many :roster_items, RosterItem
    has_many :tokens, AuthToken
    has_many :tros_metadatas, TROSMetadata
    has_many :invite_codes, InviteCode
    has_many :sent_invitations, Invitation
    has_many :received_invitations, Invitation, foreign_key: :invitee_id
    has_many :sent_messages, Message, foreign_key: :sender_id
    has_many :received_messages, Message, foreign_key: :recipient_id

    many_to_many(:bot_subscriptions, Bot, join_through: Subscription)
  end

  @type id :: binary
  @type username :: binary
  @type resource :: binary
  @type provider :: binary
  @type external_id :: binary
  @type phone_number :: binary
  @type handle :: binary
  @type role :: binary
  @type hidden_state :: nil | DateTime.t()

  @type bot_relationship ::
          :owned | :invited | :subscribed | :visitor | :visible

  @type t :: %User{
          id: id,
          username: username,
          handle: nil | handle,
          avatar: nil | binary,
          first_name: nil | binary,
          last_name: nil | binary,
          email: nil | binary,
          tagline: nil | binary,
          provider: nil | provider,
          external_id: nil | external_id,
          phone_number: nil | phone_number,
          roles: [role],
          welcome_sent: boolean,
          hidden_until: hidden_state
        }

  @update_fields [
    :handle,
    :avatar,
    :first_name,
    :last_name,
    :email,
    :tagline,
    :roles,
    :external_id,
    :provider,
    :hidden_until
  ]

  @min_handle_len 3
  @max_handle_len 16
  @max_name_len 32

  @no_index_role "__no_index__"
  @system_role "__system__"

  @invite_code_expire_days 30

  @doc "Return the list of fields that can be updated on an existing user."
  @spec valid_update_fields :: [binary]
  def valid_update_fields do
    for field <- @update_fields, do: to_string(field)
  end

  @spec to_jid(t, binary | nil) :: JID.t()
  def to_jid(%User{id: user} = u, resource \\ nil) do
    JID.make(user, Wocky.host(), resource || (u.resource || ""))
  end

  @spec get_by_jid(JID.t()) :: t | nil
  def get_by_jid(jid(luser: "")), do: nil

  def get_by_jid(jid(luser: id, lresource: resource)) do
    case Repo.get(User, id) do
      nil -> nil
      user -> %User{user | resource: resource}
    end
  end

  @spec get_user(id, t | nil) :: t | nil
  def get_user(id, requestor \\ nil) do
    if is_nil(requestor) || !Block.blocked?(requestor.id, id) do
      Repo.get(User, id)
    end
  end

  @doc "Returns all bots that the user subscribes to"
  @spec get_subscriptions(t) :: [Bot.t()]
  def get_subscriptions(user) do
    user
    |> subscribed_bots_query()
    |> Repo.all()
  end

  @spec bot_count(User.t()) :: non_neg_integer
  def bot_count(user) do
    user
    |> Ecto.assoc(:bots)
    |> where(pending: false)
    |> select([b], count(b.id))
    |> Repo.one!()
  end

  @spec owns?(t, Bot.t()) :: boolean
  def owns?(user, bot), do: user.id == bot.user_id

  @spec can_access?(t, Bot.t()) :: boolean
  def can_access?(user, bot),
    do:
      owns?(user, bot) || Invitation.invited?(bot, user) ||
        Subscription.state(user, bot) != nil

  @doc """
    Returns true if a bot should appear in a user's geosearch results. Criteria:
      * Bots user owns
      * Bots user is subscribed to
  """
  @spec searchable?(t, Bot.t()) :: boolean
  def searchable?(user, bot) do
    Bot.subscription(bot, user) != nil
  end

  @doc "Returns all bots that the user owns"
  @spec get_owned_bots(t) :: [Bot.t()]
  def get_owned_bots(user) do
    user
    |> owned_bots_query()
    |> order_by(asc: :updated_at)
    |> Repo.all()
  end

  @doc """
  Update the data on an existing user.
  Fields is a map containing fields to update.
  """
  @spec update(id | t, map) :: {:ok, t} | {:error, term}
  def update(%User{} = user, fields) do
    changeset = changeset(user, fields)

    case Repo.update(changeset) do
      {:ok, user} ->
        maybe_send_welcome(user)
        {:ok, user}

      {:error, _} = error ->
        error
    end
  end

  def update(id, fields) do
    case Repo.get(User, id) do
      nil ->
        {:error, :user_not_found}

      struct ->
        User.update(struct, fields)
    end
  end

  def changeset(user, params) do
    user
    |> cast(params, @update_fields)
    |> validate_change(:email, &validate_email/2)
    |> validate_length(:handle, min: @min_handle_len, max: @max_handle_len)
    |> validate_change(:handle, &validate_handle/2)
    |> validate_length(:first_name, max: @max_name_len)
    |> validate_length(:last_name, max: @max_name_len)
    |> validate_change(:first_name, &validate_name/2)
    |> validate_change(:last_name, &validate_name/2)
    |> validate_change(:avatar, &validate_avatar(&1, user, &2))
    |> unique_constraint(:handle, name: :users_lower_handle_index)
    |> prepare_changes(fn changeset ->
      Avatar.maybe_delete_existing(changeset.changes[:avatar], user)
      changeset
    end)
  end

  defp validate_email(:email, email) do
    if EmailChecker.valid?(email) do
      []
    else
      [email: "address is invalid"]
    end
  end

  defp validate_handle(:handle, handle) do
    reserved =
      Enum.any?(
        reserved_handles(),
        &Regex.match?(~r/.*#{&1}.*/, String.downcase(handle))
      )

    cond do
      reserved ->
        [handle: "unavailable"]

      Regex.run(~r/[a-zA-Z0-9_]+/, handle) != [handle] ->
        [handle: "invalid characters"]

      true ->
        []
    end
  end

  defp reserved_handles, do: Application.get_env(:wocky, :reserved_handles, [])

  defp validate_name(field, name) do
    if Regex.run(validation_regex(), name) != [name] do
      [{field, "invalid characters"}]
    else
      []
    end
  end

  # regexs implementing the rules at
  # https://github.com/hippware/tr-wiki/wiki/
  #     User-fields-validation-discussion#first-name-last-name
  #
  # The ?! and ?<! blocks provide negated lookaround checks for characters
  # at the start and end of the string without consuming them.
  # \p{L*} cover the three unicode categories we accept (lowercase, uppercase,
  # and other characters). \p{Z} maintains any whitespace in our search term
  # (which is removed later when we split the tokens).
  defp validation_regex do
    ~r|(?![ \-0-9])[\p{Ll}\p{Lu}\p{Lo} \-'0-9]*(?<![ \-])|u
  end

  defp search_cleanup_regex do
    ~r|[^\-0-9\p{Ll}\p{Lu}\p{Lo}\p{Z}]|u
  end

  defp validate_avatar(:avatar, user, avatar) do
    case Avatar.validate(user, avatar) do
      {:ok, _} ->
        []

      {:error, :not_found} ->
        [avatar: "does not exist"]

      {:error, :invalid_file} ->
        [avatar: "has an invalid file name (must be UUID)"]

      {:error, :invalid_url} ->
        [avatar: "is an invalid file URL"]

      {:error, :not_file_owner} ->
        [avatar: "is not owned by the user"]
    end
  end

  @spec hide(t(), boolean() | DateTime.t()) :: {:ok, t()} | {:error, term}
  def hide(user, false), do: User.update(user, %{hidden_until: nil})
  def hide(user, true), do: User.update(user, %{hidden_until: @forever})
  def hide(user, expiry), do: User.update(user, %{hidden_until: expiry})

  @spec hidden?(t()) :: boolean()
  def hidden?(user), do: user |> hidden_state() |> elem(0)

  @spec hidden_state(t()) :: {boolean(), DateTime.t()}
  def hidden_state(user) do
    case user.hidden_until do
      nil -> {false, nil}
      @forever -> {true, nil}
      until -> {DateTime.compare(DateTime.utc_now(), until) != :gt, until}
    end
  end

  @spec make_invite_code(User.t()) :: binary
  def make_invite_code(user) do
    code = InviteCode.generate()

    user
    |> Ecto.build_assoc(:invite_codes)
    |> InviteCode.changeset(%{code: code})
    |> Repo.insert!()

    code
  end

  @spec redeem_invite_code(User.t(), binary) :: boolean
  def redeem_invite_code(redeemer, code) do
    invitation =
      InviteCode
      |> where(code: ^code)
      |> preload(:user)
      |> Block.object_visible_query(redeemer.id)
      |> Repo.one()

    do_redeem_invite_code(redeemer, invitation)
  end

  defp do_redeem_invite_code(_, nil), do: false

  defp do_redeem_invite_code(redeemer, %InviteCode{user: inviter} = invitation),
    do: do_redeem_invite_code(redeemer, inviter, invitation)

  defp do_redeem_invite_code(%User{id: id}, %User{id: id}, _), do: true

  defp do_redeem_invite_code(redeemer, inviter, invitation) do
    ts = Timex.shift(invitation.created_at, days: @invite_code_expire_days)

    if Timex.after?(DateTime.utc_now(), ts) do
      # Code has expired
      false
    else
      :ok = Roster.befriend(redeemer.id, inviter.id)
      true
    end
  end

  @spec get_locations_query(t, resource) :: Queryable.t()
  def get_locations_query(user, resource) do
    user
    |> Ecto.assoc(:locations)
    |> where(resource: ^resource)
  end

  @spec get_location_events_query(t, resource | Location.t()) :: Queryable.t()
  def get_location_events_query(_user, %Location{} = loc) do
    Ecto.assoc(loc, :events)
  end

  def get_location_events_query(user, resource) when is_binary(resource) do
    user
    |> Ecto.assoc(:bot_events)
    |> where(resource: ^resource)
  end

  @spec set_location(t, resource, float, float, float, boolean) ::
          :ok | {:error, any}
  def set_location(user, resource, lat, lon, accuracy, is_fetch \\ false) do
    location = %Location{
      lat: lat,
      lon: lon,
      accuracy: accuracy,
      is_fetch: is_fetch,
      resource: resource
    }

    with {:ok, _} <- set_location(user, location) do
      :ok
    end
  end

  @doc """
  Sets the user's current location to the provided Location struct and runs the
  geofence calculation for all of the user's subscribed bots.
  """
  @spec set_location(t, Location.t()) :: {:ok, Location.t()} | {:error, any}
  def set_location(user, location) do
    with {:ok, loc} = result <- insert_location(user, location) do
      if !hidden?(user),
        do: GeoFence.check_for_bot_events(loc, user)

      result
    end
  end

  @doc """
  Sets the user's current location to the provided Location struct and runs the
  geofence calculation for the specified bot only and with debouncing disabled.
  """
  @spec set_location_for_bot(t, Location.t(), Bot.t()) ::
          {:ok, Location.t()} | {:error, any}
  def set_location_for_bot(user, location, bot) do
    with {:ok, loc} = result <- insert_location(user, location) do
      if !hidden?(user),
        do: GeoFence.check_for_bot_event(bot, loc, user)

      result
    end
  end

  def insert_location(user, location) do
    {nlat, nlon} = GeoUtils.normalize_lat_lon(location.lat, location.lon)
    captured_at = normalize_captured_at(location)
    nloc = %Location{location | lat: nlat, lon: nlon, captured_at: captured_at}

    user
    |> Ecto.build_assoc(:locations)
    |> Location.changeset(Map.from_struct(nloc))
    |> Repo.insert()
  end

  defp normalize_captured_at(%Location{captured_at: time})
       when not is_nil(time),
       do: time

  defp normalize_captured_at(_), do: DateTime.utc_now()

  @doc "Removes the user from the database"
  @spec delete(id) :: :ok | no_return
  def delete(id) do
    user = Repo.get(User, id)

    user && Repo.delete!(user)
    :ok
  end

  @spec add_role(id, role) :: :ok
  def add_role(id, role) do
    User
    |> where(username: ^id)
    |> where([q], ^role not in q.roles)
    |> Repo.update_all(push: [roles: role])

    :ok
  end

  @spec remove_role(id, role) :: :ok
  def remove_role(id, role) do
    User
    |> where(username: ^id)
    |> Repo.update_all(pull: [roles: role])

    :ok
  end

  @spec owned_bots_query(User.t()) :: Queryable.t()
  def owned_bots_query(user) do
    user
    |> Ecto.assoc(:bots)
    |> where(pending: false)
  end

  @spec subscribed_bots_query(User.t()) :: Queryable.t()
  def subscribed_bots_query(user) do
    Bot
    |> where(pending: false)
    |> join(
      :left,
      [b],
      s in Subscription,
      b.id == s.bot_id and s.user_id == ^user.id
    )
    |> where([b, s], not is_nil(s.user_id))
  end

  @spec filter_hidden(Queryable.t()) :: Queryable.t()
  def filter_hidden(query) do
    query
    |> where(
      [u, ...],
      is_nil(u.hidden_until) or u.hidden_until < ^DateTime.utc_now()
    )
  end

  @doc "Generate a full name for anywhere it needs pretty-printing"
  @spec full_name(User.t()) :: String.t()
  def full_name(user), do: String.trim("#{user.first_name} #{user.last_name}")

  def no_index_role, do: @no_index_role
  def system_role, do: @system_role

  def remove_auth_details(id) do
    User
    |> where(id: ^id)
    |> Repo.update_all(
      set: [phone_number: nil, provider: nil, external_id: nil]
    )

    :ok
  end

  @spec search_by_name(binary, id, non_neg_integer) :: [User.t()]
  def search_by_name("", _, _), do: []

  def search_by_name(search_prefix, user_id, limit) do
    search_term =
      search_cleanup_regex()
      |> Regex.replace(search_prefix, "")
      |> String.split()
      |> Enum.map(&Kernel.<>(&1, ":*"))
      |> Enum.join(" & ")

    User
    |> where(
      fragment(
        """
        users_name_fts(first_name, last_name, handle)
        @@ to_tsquery('simple', unaccent(?))
        """,
        ^search_term
      )
    )
    |> Block.object_visible_query(user_id, :id)
    |> where([u], u.id != ^user_id)
    |> limit(^limit)
    |> Repo.all()
  end

  @spec get_bot_relationships(t(), Bot.t()) :: [bot_relationship()]
  def get_bot_relationships(user, bot) do
    sub = Subscription.get(user, bot)

    [:visible]
    |> maybe_add_rel(bot.user_id == user.id, :owned)
    |> maybe_add_rel(Invitation.invited?(bot, user), :invited)
    |> maybe_add_rel(sub != nil, [:subscribed])
    |> maybe_add_rel(sub != nil && sub.visitor, :visitor)
    |> List.flatten()
  end

  def forever_ts, do: @forever

  defp maybe_add_rel(list, true, rel), do: [rel | list]
  defp maybe_add_rel(list, false, _rel), do: list

  defp maybe_send_welcome(%User{welcome_sent: true}), do: :ok
  defp maybe_send_welcome(%User{email: nil}), do: :ok

  defp maybe_send_welcome(%User{} = user) do
    if Confex.get_env(:wocky, :send_welcome_email) do
      send_welcome_email(user)
    else
      :ok
    end
  end

  defp send_welcome_email(user) do
    Email.send_welcome_email(user)

    user
    |> cast(%{welcome_sent: true}, [:welcome_sent])
    |> Repo.update()
  end
end
