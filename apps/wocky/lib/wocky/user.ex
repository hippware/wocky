defmodule Wocky.User do
  @moduledoc ""

  use Wocky.Repo.Model

  import OK, only: ["~>>": 2]

  alias Wocky.Device
  alias Wocky.Index
  alias Wocky.Repo.ID
  alias Wocky.Token
  alias __MODULE__, as: User

  @primary_key {:id, :binary_id, autogenerate: false}
  schema "users" do
    field :username,     :string # User ID (userpart of JID)
    field :server,       :string # User Server (domainpart of JID)
    field :resource,     :string, virtual: true
    field :external_id,  :string # The user ID received from Twitter Digits
    field :handle,       :string # User handle (as seen by other users)
    field :avatar,       :string # ID of file containing user's avatar
    field :first_name,   :string # User's first name
    field :last_name,    :string # User's last name
    field :phone_number, :string # The user's phone number (also from digits)
    field :email,        :string # User's email address
    field :password,     :string # Password hash
    field :pass_details, :string

    timestamps()

    has_many :tokens, Token
    has_many :devices, Device
  end

  @type id           :: binary
  @type username     :: binary
  @type server       :: binary
  @type resource     :: binary
  @type external_id  :: binary
  @type phone_number :: binary
  @type handle       :: binary

  @type t :: %User{
    id:             id,
    username:       username,
    server:         server,
    handle:         nil | handle,
    avatar:         nil | binary,
    first_name:     nil | binary,
    last_name:      nil | binary,
    email:          nil | binary,
    external_id:    nil | external_id,
    phone_number:   nil | phone_number,
  }

  @change_fields [:handle, :avatar, :first_name, :last_name, :email]

  @doc """
  Creates or updates a user based on the external authentication ID and
  phone number.
  """
  @spec register(server, external_id, phone_number) ::
    {:ok, {username, server, boolean}} | no_return
  def register(server, external_id, phone_number) do
    case Repo.get_by(User, external_id: external_id) do
      nil ->
        username = ID.new
        %User{
          id: username,
          username: username,
          server: server,
          external_id: external_id,
          phone_number: phone_number
        }
        |> Repo.insert!

        {:ok, {username, server, true}}

      user ->
        {:ok, {user.username, user.server, false}}
    end
  end

  @doc """
  Returns a map of all fields for a given user or `nil' if no such
  user exists.
  """
  @spec find(id) :: t | nil
  def find(id) do
    Repo.get(User, id)
  end

  @doc "Search for a user based on the value of a property"
  @spec find_by(:handle | :phone_number | :external_id, binary) :: t | nil
  def find_by(field, value) do
    Repo.get_by(User, [{field, value}])
  end

  defp with_user(query, username) do
    from u in query, where: u.username == ^username
  end

  defp select_handle(query) do
    from u in query, select: u.handle
  end

  defp select_phone_number(query) do
    from u in query, select: u.phone_number
  end

  @doc "Returns the user's handle"
  @spec get_handle(id) :: handle | nil
  def get_handle(id) do
    User
    |> with_user(id)
    |> select_handle
    |> Repo.one
  end

  @doc "Returns the user's phone number"
  @spec get_phone_number(id) :: phone_number | nil
  def get_phone_number(id) do
    User
    |> with_user(id)
    |> select_phone_number
    |> Repo.one
  end

  @doc """
  Update the data on an existing user.
  Fields is a map containing fields to update.
  """
  @spec update(id, map) :: :ok | {:error, term}
  def update(id, fields) do
    User
    |> Repo.get!(id)
    |> changeset(fields)
    |> Repo.update
    ~>> do_update_index(fields)
  end

  defp changeset(struct, params) do
    struct
    |> cast(params, @change_fields)
    |> validate_format(:email, ~r/@/)
    |> validate_exclusion(:handle, reserved_handles())
    |> unique_constraint(:handle)
    |> unique_constraint(:external_id)
  end

  defp reserved_handles,
    do: Application.get_env(:wocky, :reserved_handles, [])

  defp do_update_index(%User{username: username}, fields) do
    :ok = Index.user_updated(username, fields)
  end

  @doc "Removes the user from the database"
  @spec delete(id) :: :ok | no_return
  def delete(id) do
    User
    |> with_user(id)
    |> Repo.delete_all

    :ok = Index.user_removed(id)
  end

  # =========================================================================

  # def set_avatar(user, avatar) do
  #   Avatar.prepare(avatar)
  #   ~>> Avatar.check_is_local(user.server)
  #   ~>> Avatar.check_owner(user.id)
  #   ~>> Avatar.delete_existing(user.avatar)
  #   ~>> Avatar.to_url
  #   ~>> do_set_avatar(user)
  # end

  # defp do_set_avatar(avatar, user) do
  #   {:ok, %User{user | avatar: avatar}}
  # end

  # @spec set_location(t, Location.t) :: {:ok, t}
  # def set_location(user, location) do
  #   do_set_location(user, location)
  #   ~>> Location.check_for_bot_events(location)
  #   ~>> Location.update_bot_locations(location)
  # end

  # defp do_set_location(user, location) do
  #   %User{user | location: Map.from_struct(location)}
  # end

  # @spec to_jid(t, binary | nil) :: Ejabberd.jid
  # def to_jid(%User{id: user, server: server} = u, resource \\ nil) do
  #   Ejabberd.make_jid!(user, server, resource || (u.resource || ""))
  # end

  # @spec to_jid_string(t, binary | nil) :: binary
  # def to_jid_string(%User{} = user, resource \\ nil) do
  #   user |> to_jid(resource) |> :jid.to_binary
  # end

  # @spec to_bare_jid(t) :: Ejabberd.jid
  # def to_bare_jid(%User{} = user) do
  #   user |> to_jid |> :jid.to_bare
  # end

  # @spec to_bare_jid_string(t) :: binary
  # def to_bare_jid_string(%User{} = user) do
  #   user |> to_bare_jid |> :jid.to_binary
  # end

  # @spec from_jid(binary, binary, binary) :: t
  # def from_jid(user, server, resource) do
  #   %User{id: user, server: server, resource: resource}
  # end

  # @spec from_jid(Ejabberd.jid) :: t
  # def from_jid(jid) do
  #   jid(user: user, server: server, resource: resource) = jid
  #   from_jid(user, server, resource)
  # end

  # @spec get_subscribed_bots(t) :: [Ejabberd.jid]
  # def get_subscribed_bots(user) do
  #   :wocky_db_bot.subscribed_bots(to_jid(user))
  # end

  # @spec get_owned_bots(t) :: [Bot.t]
  # def get_owned_bots(user) do
  #   :all
  #   |> Schemata.select(
  #       from: :user_bot, in: :wocky_db.shared_keyspace,
  #       where: %{owner: to_bare_jid_string(user)})
  #   |> Enum.map(&Bot.new(&1))
  # end

  # @spec get_last_bot_event(t, binary) :: [map]
  # def get_last_bot_event(user, bot_id) do
  #   Schemata.select :all,
  #     from: :bot_event, in: :wocky_db.local_keyspace,
  #     where: %{jid: to_jid_string(user), bot: bot_id},
  #     limit: 1
  # end

  # @spec add_bot_event(t, binary, :enter | :exit) :: boolean
  # def add_bot_event(user, bot_id, event) do
  #   Schemata.insert into: :bot_event, in: :wocky_db.local_keyspace,
  #     values: %{
  #       jid: to_jid_string(user),
  #       bot: bot_id,
  #       event: event,
  #       created_at: :now
  #     }
  # end
end
