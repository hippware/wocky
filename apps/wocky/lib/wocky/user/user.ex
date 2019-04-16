defmodule Wocky.User do
  @moduledoc "Schema and API for working with users."

  require Logger

  use Wocky.Repo.Schema

  import Ecto.Query

  alias Ecto.{Changeset, Queryable}
  alias FirebaseAdminEx.Auth, as: FirebaseAuth
  alias Wocky.Account.ClientVersion

  alias Wocky.{
    Block,
    Bot,
    Conversation,
    Message,
    Notifier,
    Repo,
    Roster,
    TROS
  }

  alias Wocky.Bot.{Invitation, Subscription}
  alias Wocky.Events.NewUser
  alias Wocky.Notifier.Push.Token, as: PushToken
  alias Wocky.Roster.Item, as: RosterItem
  alias Wocky.TROS.Metadata, as: TROSMetadata

  alias Wocky.User.{
    Avatar,
    BotEvent,
    CurrentLocation,
    InviteCode,
    Location,
    LocationShare,
    Presence
  }

  @forever "2200-01-01T00:00:00.000000Z" |> DateTime.from_iso8601() |> elem(1)

  @primary_key {:id, :binary_id, autogenerate: false}
  schema "users" do
    # Unique ID of the currently logged-in device.
    # I am not sure if this is the right place to track this information,
    # but it is currently the path of least resistance.
    field :device, :string, virtual: true
    # The external auth provider
    field :provider, :string
    # The user ID received from the provider
    field :external_id, :string
    # User handle (as seen by other users)
    field :handle, :string
    # TROS URL of file containing user's avatar
    field :image_url, :string
    # User's name
    field :name, :string
    # The user's phone number
    field :phone_number, :string
    # (also from auth provider)
    # User's email address
    field :email, :string
    # User's tagline
    field :tagline, :string
    # Password hash
    field :password, :string
    # Client data blob (opaque to server)
    field :client_data, :string
    field :pass_details, :string
    field :roles, {:array, :string}, default: []
    field :welcome_sent, :boolean
    field :smss_sent, :integer
    field :bot_created, :boolean

    field :presence, :any, virtual: true

    timestamps()

    has_one :current_location, CurrentLocation

    has_many :bots, Bot
    has_many :bot_events, BotEvent
    has_many :conversations, Conversation
    has_many :locations, Location
    has_many :push_tokens, PushToken
    has_many :roster_contacts, RosterItem, foreign_key: :contact_id
    has_many :roster_items, RosterItem
    has_many :client_versions, ClientVersion
    has_many :tros_metadatas, TROSMetadata
    has_many :invite_codes, InviteCode
    has_many :sent_invitations, Invitation
    has_many :received_invitations, Invitation, foreign_key: :invitee_id
    has_many :sent_messages, Message, foreign_key: :sender_id
    has_many :received_messages, Message, foreign_key: :recipient_id

    many_to_many(:bot_subscriptions, Bot, join_through: Subscription)
  end

  @type id :: binary
  @type device :: binary
  @type provider :: binary
  @type external_id :: binary
  @type phone_number :: binary
  @type handle :: binary
  @type role :: binary

  @type bot_relationship ::
          :owned | :invited | :subscribed | :visitor | :visible

  @type t :: %User{
          id: id,
          handle: nil | handle,
          image_url: nil | binary,
          name: nil | binary,
          email: nil | binary,
          tagline: nil | binary,
          provider: nil | provider,
          external_id: nil | external_id,
          phone_number: nil | phone_number,
          roles: [role],
          welcome_sent: boolean,
          smss_sent: integer,
          bot_created: boolean,
          presence: nil | Presence.t()
        }

  @update_fields [
    :handle,
    :image_url,
    :name,
    :email,
    :tagline,
    :roles,
    :external_id,
    :provider,
    :smss_sent,
    :bot_created,
    :client_data
  ]

  @min_handle_len 3
  @max_handle_len 16
  @max_name_len 65

  @no_index_role "__no_index__"
  @system_role "__system__"

  @invite_code_expire_days 30

  # ----------------------------------------------------------------------
  # Utilities

  @doc "Returns true if the user is a Hippware employee"
  @spec hippware?(t) :: boolean
  def hippware?(%User{email: email}),
    do: email && String.ends_with?(email, "@hippware.com")

  def hippware?(_), do: false

  def first_name(%User{name: name}),
    do: name |> split_name() |> elem(0)

  def last_name(%User{name: name}),
    do: name |> split_name() |> elem(1)

  def no_index_role, do: @no_index_role
  def system_role, do: @system_role
  def forever_ts, do: @forever

  defp split_name(nil), do: {"", ""}
  defp split_name(""), do: {"", ""}

  defp split_name(name) do
    last =
      name
      |> String.split(" ", trim: true)
      |> List.last()

    first =
      name
      |> String.trim()
      |> String.replace_suffix(last, "")
      |> String.trim()

    {first, last}
  end

  # ----------------------------------------------------------------------
  # Database interaction

  @spec get_user(id, t | nil) :: t | nil
  def get_user(id, requestor \\ nil) do
    if is_nil(requestor) || !Block.blocked?(requestor.id, id) do
      Repo.get(User, id)
    end
  end

  @spec get_by_phone_number([String.t()], t) :: [t]
  def get_by_phone_number(phone_numbers, requestor) do
    User
    |> where([u], u.phone_number in ^phone_numbers)
    |> Block.object_visible_query(requestor, :id)
    |> Repo.all()
  end

  @doc """
  Update the data on an existing user.
  Fields is a map containing fields to update.
  """
  @spec update(id | t, map) :: {:ok, t} | {:error, term}
  def update(%User{} = user, fields) do
    changeset = changeset(user, fields)

    with {:ok, updated_user} <- Repo.update(changeset) do
      maybe_send_welcome(updated_user)
      {:ok, updated_user}
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

  @doc """
  Check if the user is allowed to send an SMS and increment the count
  of those sent
  """
  @spec sms_allowed_inc?(t()) :: boolean()
  def sms_allowed_inc?(user) do
    with %User{} = u <- get_user(user.id) do
      if u.smss_sent < Confex.get_env(:wocky, :max_sms_per_user) do
        {:ok, _} = __MODULE__.update(user.id, %{smss_sent: u.smss_sent + 1})
        true
      else
        allowed_unlimited_smss?(user)
      end
    else
      _ -> false
    end
  end

  defp allowed_unlimited_smss?(user) do
    :wocky
    |> Confex.get_env(:unlimited_sms_numbers)
    |> Enum.member?(user.phone_number)
  end

  defp maybe_send_welcome(%User{welcome_sent: true}), do: :ok
  defp maybe_send_welcome(%User{email: nil}), do: :ok

  defp maybe_send_welcome(%User{} = user) do
    Notifier.notify(%NewUser{user: user})
  end

  def remove_auth_details(id) do
    User
    |> where(id: ^id)
    |> Repo.update_all(
      set: [phone_number: nil, provider: nil, external_id: nil]
    )

    :ok
  end

  @doc "Removes the user from the database"
  @spec delete(id) :: :ok
  def delete(id) do
    user = Repo.get(User, id)

    if user do
      TROS.delete_all(user)

      _ =
        if user.provider == "firebase",
          do: FirebaseAuth.delete_user(user.external_id)

      Repo.delete!(user)
    end

    :ok
  end

  @doc "Mark the user as having created a bot at some point in their life"
  @spec flag_bot_created(t) :: :ok
  def flag_bot_created(%{bot_created: true}), do: :ok

  def flag_bot_created(user) do
    user
    |> changeset(%{bot_created: true})
    |> Repo.update()

    :ok
  end

  # ----------------------------------------------------------------------
  # Validations

  def changeset(user, params) do
    user
    |> cast(params, @update_fields)
    |> validate_change(:email, &validate_email/2)
    |> validate_length(:handle, min: @min_handle_len, max: @max_handle_len)
    |> validate_change(:handle, &validate_handle/2)
    |> validate_length(:name, max: @max_name_len)
    |> validate_change(:name, &validate_name/2)
    |> validate_change(:image_url, &validate_avatar(&1, user, &2))
    |> unique_constraint(:handle, name: :users_lower_handle_index)
    |> prepare_changes(fn changeset ->
      Avatar.maybe_delete_existing(changeset.changes[:image_url], user)
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
    ~r|(?![ \-0-9])[\p{Ll}\p{Lu}\p{Lo} \-\.'0-9]*(?<![ \-])|u
  end

  defp search_cleanup_regex do
    ~r|[^\-0-9\p{Ll}\p{Lu}\p{Lo}\p{Z}]|u
  end

  defp validate_avatar(:image_url, user, avatar) do
    case Avatar.validate(user, avatar) do
      {:ok, _} ->
        []

      {:error, :not_found} ->
        [image_url: "does not exist"]

      {:error, :invalid_file} ->
        [image_url: "has an invalid file name (must be UUID)"]

      {:error, :invalid_url} ->
        [image_url: "is an invalid file URL"]

      {:error, :not_file_owner} ->
        [image_url: "is not owned by the user"]
    end
  end

  # ----------------------------------------------------------------------
  # Bot relationships

  @doc "Returns all bots that the user owns"
  @spec get_owned_bots(t) :: [Bot.t()]
  def get_owned_bots(user) do
    user
    |> owned_bots_query()
    |> order_by(asc: :updated_at)
    |> Repo.all()
  end

  @spec owned_bots_query(User.t()) :: Queryable.t()
  def owned_bots_query(user) do
    user
    |> Ecto.assoc(:bots)
    |> where(pending: false)
  end

  @doc "Returns all bots that the user subscribes to"
  @spec get_subscriptions(t) :: [Bot.t()]
  def get_subscriptions(user) do
    Bot
    |> where(pending: false)
    |> join(
      :left,
      [b],
      s in Subscription,
      on: b.id == s.bot_id and s.user_id == ^user.id
    )
    |> where([b, s], not is_nil(s.user_id))
    |> Repo.all()
  end

  @spec can_access?(t, Bot.t()) :: boolean
  def can_access?(user, bot) do
    owns?(user, bot) || Invitation.invited?(bot, user) ||
      Subscription.state(user, bot) != nil
  end

  defp owns?(user, bot), do: user.id == bot.user_id

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

  defp maybe_add_rel(list, true, rel), do: [rel | list]
  defp maybe_add_rel(list, false, _rel), do: list

  # ----------------------------------------------------------------------
  # Invite codes

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
      |> Block.object_visible_query(redeemer)
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
      :ok = Roster.befriend(redeemer, inviter)
      true
    end
  end

  # ----------------------------------------------------------------------
  # Location

  @spec get_locations_query(t, device) :: Queryable.t()
  def get_locations_query(user, device) do
    user
    |> Ecto.assoc(:locations)
    |> where(device: ^device)
  end

  @spec get_location_events_query(t, device | Location.t()) :: Queryable.t()
  def get_location_events_query(_user, %Location{} = loc) do
    Ecto.assoc(loc, :events)
  end

  def get_location_events_query(user, device) when is_binary(device) do
    user
    |> Ecto.assoc(:bot_events)
    |> where(device: ^device)
  end

  @spec set_location(t, device, float, float, float) :: :ok | {:error, any}
  def set_location(user, device, lat, lon, accuracy) do
    location = %Location{
      lat: lat,
      lon: lon,
      accuracy: accuracy,
      device: device
    }

    with {:ok, _} <- set_location(user, location) do
      :ok
    end
  end

  @doc """
  Sets the user's current location to the provided Location struct and runs the
  geofence calculation for all of the user's subscribed bots.
  """
  @spec set_location(t, Location.t(), boolean()) ::
          {:ok, Location.t()} | {:error, any}
  def set_location(user, location, current? \\ true),
    do: Location.set_location(user, location, current?)

  @doc """
  Sets the user's current location to the provided Location struct and runs the
  geofence calculation for the specified bot only and with debouncing disabled.
  """
  @spec set_location_for_bot(t, Location.t(), Bot.t()) ::
          {:ok, Location.t()} | {:error, any}
  def set_location_for_bot(user, location, bot),
    do: Location.set_location_for_bot(user, location, bot)

  @doc "Gets the current location for the user."
  @spec get_current_location(User.t()) :: Location.t() | nil
  def get_current_location(user) do
    Repo.get(CurrentLocation, user.id)
  end

  @spec start_sharing_location(User.t(), User.t(), DateTime.t()) ::
          {:ok, LocationShare.t()} | {:error, Changeset.t() | atom}
  def start_sharing_location(user, shared_with, expiry) do
    %LocationShare{}
    |> LocationShare.changeset(%{
      user_id: user.id,
      shared_with_id: shared_with.id,
      expires_at: expiry
    })
    |> Repo.insert(
      on_conflict: [set: [expires_at: expiry, updated_at: DateTime.utc_now()]],
      conflict_target: [:user_id, :shared_with_id]
    )
  end

  @spec stop_sharing_location(User.t(), User.t()) :: :ok
  def stop_sharing_location(%User{id: user_id}, %User{id: shared_with_id}) do
    LocationShare
    |> where(user_id: ^user_id, shared_with_id: ^shared_with_id)
    |> Repo.delete_all()

    :ok
  end

  @spec stop_sharing_location(User.t()) :: :ok
  def stop_sharing_location(%User{id: user_id}) do
    LocationShare
    |> where(user_id: ^user_id)
    |> Repo.delete_all()

    :ok
  end

  @spec get_location_shares(User.t()) :: [LocationShare.t()]
  def get_location_shares(user) do
    user
    |> get_location_shares_query()
    |> Repo.all()
  end

  @spec get_location_shares_query(User.t()) :: Queryable.t()
  def get_location_shares_query(%User{id: user_id}) do
    location_shares_query()
    |> where([ls], ls.user_id == ^user_id)
  end

  @spec get_location_sharers(User.t()) :: [LocationShare.t()]
  def get_location_sharers(user) do
    user
    |> get_location_sharers_query()
    |> Repo.all()
  end

  @spec get_location_sharers_query(User.t()) :: Queryable.t()
  def get_location_sharers_query(%User{id: user_id}) do
    location_shares_query()
    |> where([ls], ls.shared_with_id == ^user_id)
  end

  defp location_shares_query do
    now = DateTime.utc_now()

    LocationShare
    |> preload([:user, :shared_with])
    |> where([ls], ls.expires_at > ^now)
    |> order_by([ls], desc: ls.created_at)
  end

  # ----------------------------------------------------------------------
  # Searching

  @spec search_by_name(binary, t(), non_neg_integer) :: [User.t()]
  def search_by_name("", _, _), do: []

  def search_by_name(search_prefix, user, limit) do
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
        users_name_fts(name, handle)
        @@ to_tsquery('simple', unaccent(?))
        """,
        ^search_term
      )
    )
    |> Block.object_visible_query(user, :id)
    |> where([u], u.id != ^user.id)
    |> limit(^limit)
    |> Repo.all()
  end
end
