defmodule Wocky.User do
  @moduledoc ""

  require Logger

  use Wocky.JID
  use Wocky.Repo.Schema

  import Ecto.Query
  import OK, only: [~>>: 2]

  alias Ecto.Queryable
  alias Wocky.Bot
  alias Wocky.Bot.Share
  alias Wocky.Bot.Subscription
  alias Wocky.Conversation
  alias Wocky.Email
  alias Wocky.HomeStreamItem
  alias Wocky.Index
  alias Wocky.Push.Token, as: PushToken
  alias Wocky.Repo
  alias Wocky.Repo.ID
  alias Wocky.RosterItem
  alias Wocky.Token, as: AuthToken
  alias Wocky.TROS.Metadata, as: TROSMetadata
  alias Wocky.User.Avatar
  alias Wocky.User.BotEvent
  alias Wocky.User.Location
  alias __MODULE__

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

  @register_fields [
    :username,
    :server,
    :provider,
    :external_id,
    :phone_number,
    :password,
    :pass_details
  ]
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
  @max_register_retries 5

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

  @doc """
  Creates a new user with a password.
  Used for testing only.
  """
  @spec register(username, server, binary, binary) :: {:ok, t} | {:error, any}
  def register(username, server, password, pass_details) do
    %{
      username: username,
      server: server,
      provider: "local",
      external_id: username,
      password: password,
      pass_details: pass_details
    }
    |> register_changeset()
    |> Repo.insert()
  end

  @doc """
  Creates or updates a user based on the external authentication ID and
  phone number.
  """
  @spec register_external(server, provider, external_id, phone_number) ::
          {:ok, {username, server, boolean}} | no_return
  def register_external(server, provider, external_id, phone_number) do
    {:ok, do_register(server, provider, external_id, phone_number, 0)}
  end

  # There's scope for a race condition here. Since the database uses "read
  # committed"
  # (see https://www.postgresql.org/docs/current/static/transaction-iso.html)
  # we can't SELECT then INSERT and assume something hasn't been inserted
  # in the interim, even in a transaction. Thus we implement a retry system
  # here - if the user was inserted after the SELECT, the next time around it
  # should work fine (unless it gets deleted in the interim in which case
  # what the heck is even going on?). Either way, if we fail after 5 retries
  # there's something seriously weird going on and raising an exception
  # is a pretty reasonable response.
  defp do_register(_, _, _, _, @max_register_retries) do
    raise "Exceeded maximum register retries"
  end

  defp do_register(server, provider, external_id, phone_number, retries) do
    case Repo.get_by(User, external_id: external_id, provider: provider) do
      nil ->
        case Repo.get_by(User, phone_number: phone_number) do
          nil ->
            register_new(server, provider, external_id, phone_number, retries)

          user ->
            __MODULE__.update(user.id, %{
              provider: provider,
              external_id: external_id,
              phone_number: phone_number
            })

            {user.username, user.server, false}
        end

      user ->
        {user.username, user.server, false}
    end
  end

  defp register_new(server, provider, external_id, phone_number, retries) do
    user = %{
      username: ID.new(),
      server: server,
      provider: provider,
      external_id: external_id,
      phone_number: phone_number
    }

    result =
      user
      |> register_changeset()
      |> Repo.insert()

    case result do
      {:ok, _} ->
        {user.username, user.server, true}

      {:error, e} ->
        Logger.debug(fn -> "registration failed with error: #{inspect(e)}" end)

        do_register(server, provider, external_id, phone_number, retries + 1)
    end
  end

  def register_changeset(params) do
    # TODO
    %User{}
    |> cast(params, @register_fields)
    |> validate_required([:username, :server, :provider, :external_id])
    |> validate_format(:phone_number, ~r//)
    |> validate_change(:username, &validate_username/2)
    |> put_change(:id, params[:username])
    |> unique_constraint(:external_id)
  end

  defp validate_username(:username, username) do
    if ID.valid?(username) do
      []
    else
      [username: "not a valid UUID"]
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
    # regex implementing the rules at
    # https://github.com/hippware/tr-wiki/wiki/
    #     User-fields-validation-discussion#first-name-last-name
    #
    # The ?! and ?<! blocks provide negated lookaround checks for characters
    # at the start and end of the string without consuming them.
    # \p{L*} cover the three unicode categories we accept (lowercase, uppercase,
    # and other characters).
    rx = ~r|(?![ \-0-9])[\p{Ll}\p{Lu}\p{Lo} \-'0-9]*(?<![ \-])|u

    if Regex.run(rx, name) != [name] do
      [{field, "invalid characters"}]
    else
      []
    end
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

    user && HomeStreamItem.delete_by_user_ref(user)
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
