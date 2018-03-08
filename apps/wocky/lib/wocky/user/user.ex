defmodule Wocky.User do
  @moduledoc ""

  require Logger

  use Wocky.JID
  use Wocky.Repo.Schema

  import Ecto.Query
  import OK, only: [~>>: 2]

  alias Ecto.Queryable
  alias Wocky.Account.Token, as: AuthToken
  alias Wocky.Blocking
  alias Wocky.Bot
  alias Wocky.Bot.Share
  alias Wocky.Bot.Subscription
  alias Wocky.Conversation
  alias Wocky.Email
  alias Wocky.HomeStream.Item, as: HomeStreamItem
  alias Wocky.Index
  alias Wocky.Push.Token, as: PushToken
  alias Wocky.Repo
  alias Wocky.Roster.Item, as: RosterItem
  alias Wocky.TROS.Metadata, as: TROSMetadata
  alias Wocky.User.Avatar
  alias Wocky.User.BotEvent
  alias Wocky.User.Location

  @primary_key {:id, :binary_id, autogenerate: false}
  schema "users" do
    # User ID (userpart of JID)
    field :username, :string
    # User Server (domainpart of JID)
    field :server, :string
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

    timestamps()

    has_many :bots, Bot
    has_many :bot_events, BotEvent
    has_many :conversations, Conversation
    has_many :home_stream_items, HomeStreamItem
    has_many :locations, Location
    has_many :push_tokens, PushToken
    has_many :roster_contacts, RosterItem, foreign_key: :contact_id
    has_many :roster_items, RosterItem
    has_many :tokens, AuthToken
    has_many :tros_metadatas, TROSMetadata

    many_to_many(:shares, Bot, join_through: Share)
    many_to_many(:subscriptions, Bot, join_through: Subscription)
  end

  @type id :: binary
  @type username :: binary
  @type server :: binary
  @type resource :: binary
  @type provider :: binary
  @type external_id :: binary
  @type phone_number :: binary
  @type handle :: binary
  @type role :: binary

  @type t :: %User{
          id: id,
          username: username,
          server: server,
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
          welcome_sent: boolean
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
    :provider
  ]

  @min_handle_len 3
  @max_handle_len 16
  @max_name_len 32

  @no_index_role "__no_index__"
  @system_role "__system__"

  @doc "Return the list of fields that can be updated on an existing user."
  @spec valid_update_fields :: [binary]
  def valid_update_fields do
    for field <- @update_fields, do: to_string(field)
  end

  @spec to_jid(t, binary | nil) :: JID.t()
  def to_jid(%User{id: user, server: server} = u, resource \\ nil) do
    JID.make(user, server, resource || (u.resource || ""))
  end

  @spec get_by_jid(JID.t()) :: t | nil
  def get_by_jid(jid(luser: "")), do: nil

  def get_by_jid(jid(luser: id, lresource: resource)) do
    case Repo.get(User, id) do
      nil -> nil
      user -> %User{user | resource: resource}
    end
  end

  @spec subscribed?(t, Bot.t()) :: boolean
  def subscribed?(user, bot) do
    owns?(user, bot) || Subscription.exists?(user, bot)
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
    do: owns?(user, bot) || Bot.public?(bot) || Share.exists?(user, bot)

  @doc """
    Returns true if a bot should appear in a user's geosearch results. Criteria:
      * Bots user owns
      * Bots user is subscribed to
  """
  @spec searchable?(t, Bot.t()) :: boolean
  def searchable?(user, bot) do
    owns?(user, bot) || Subscription.exists?(user, bot)
  end

  @doc "Returns all bots that the user owns"
  @spec get_owned_bots(t) :: [Bot.t()]
  def get_owned_bots(user) do
    user
    |> owned_bots_query()
    |> order_by(asc: :updated_at)
    |> Repo.all()
  end

  @doc "Returns all bots that the user owns and has set to 'follow me'"
  @spec get_owned_bots_with_follow_me(t) :: [Bot.t()]
  def get_owned_bots_with_follow_me(user) do
    user
    |> Ecto.assoc(:bots)
    |> with_follow_me()
    |> Repo.all()
  end

  defp with_follow_me(query) do
    from b in query,
      where: b.follow_me == ^true and b.follow_me_expiry > ^DateTime.utc_now()
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
        maybe_update_index(user)
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

  def changeset(struct, params) do
    struct
    |> cast(params, @update_fields)
    |> validate_change(:email, &validate_email/2)
    |> validate_length(:handle, min: @min_handle_len, max: @max_handle_len)
    |> validate_change(:handle, &validate_handle/2)
    |> validate_length(:first_name, max: @max_name_len)
    |> validate_length(:last_name, max: @max_name_len)
    |> validate_change(:first_name, &validate_name/2)
    |> validate_change(:last_name, &validate_name/2)
    |> validate_change(:avatar, &validate_avatar(&1, struct, &2))
    |> unique_constraint(:handle, name: :users_lower_handle_index)
    |> prepare_changes(fn changeset ->
      Avatar.maybe_delete_existing(changeset.changes[:avatar], struct.avatar)
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
    case do_validate_avatar(user, avatar) do
      {:ok, _} ->
        []

      {:error, :not_found} ->
        [avatar: "does not exist"]

      {:error, :invalid_file} ->
        [avatar: "has an invalid file name (must be UUID)"]

      {:error, :invalid_url} ->
        [avatar: "is an invalid file URL"]

      {:error, :not_local_file} ->
        [avatar: "is not a local file"]

      {:error, :not_file_owner} ->
        [avatar: "is not owned by the user"]
    end
  end

  defp do_validate_avatar(user, avatar) do
    Avatar.prepare(avatar)
    ~>> Avatar.check_valid_filename()
    ~>> Avatar.check_is_local(user.server)
    ~>> Avatar.check_owner(user.id)
  end

  @spec set_location(t, resource, float, float, float) :: :ok | {:error, any}
  def set_location(user, resource, lat, lon, accuracy) do
    case Location.insert(user, resource, lat, lon, accuracy) do
      {:ok, loc} ->
        loc
        |> Location.check_for_bot_events(user)
        |> Location.update_bot_locations(user)

        :ok

      {:error, _} = error ->
        error
    end
  end

  @doc "Removes the user from the database"
  @spec delete(id) :: :ok | no_return
  def delete(id) do
    user = Repo.get(User, id)

    user && Repo.delete!(user)
    :ok = Index.remove(:user, id)
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

  @spec searchable_bots_query(User.t()) :: Queryable.t()
  def searchable_bots_query(user) do
    Bot
    |> where([b], fragment("is_searchable(?, ?)", ^user.id, b.id))
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

  @doc "Generate a full name for anywhere it needs pretty-printing"
  @spec full_name(User.t()) :: String.t()
  def full_name(user), do: String.trim("#{user.first_name} #{user.last_name}")

  def no_index_role, do: @no_index_role
  def system_role, do: @system_role

  def remove_auth_details(id) do
    User
    |> where(id: ^id)
    |> Repo.update_all(set: [phone_number: nil,
                             provider: nil,
                             external_id: nil])
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
    |> where(fragment("""
      users_name_fts(first_name, last_name, handle)
      @@ to_tsquery('simple', unaccent(?))
      """,
      ^search_term))
    |> Blocking.object_visible_query(user_id, :id)
    |> where([u], u.id != ^user_id)
    |> limit(^limit)
    |> Repo.all()
  end

  defp maybe_send_welcome(%User{welcome_sent: true}), do: :ok
  defp maybe_send_welcome(%User{email: nil}), do: :ok

  defp maybe_send_welcome(%User{} = user) do
    Email.send_welcome_email(user)

    user
    |> cast(%{welcome_sent: true}, [:welcome_sent])
    |> Repo.update()
  end

  defp maybe_update_index(user) do
    Enum.member?(user.roles, @no_index_role) ||
      Index.update(:user, user.id, user)
  end
end
