defmodule Wocky.User do
  @moduledoc ""

  use Wocky.JID
  use Wocky.Repo.Model

  import OK, only: ["~>>": 2]

  alias Wocky.Bot
  alias Wocky.Device
  alias Wocky.Index
  alias Wocky.Repo.ID
  alias Wocky.Repo.Timestamp
  alias Wocky.Token
  alias Wocky.Conversation
  alias Wocky.TROS.Metadata, as: TROSMetadata
  alias Wocky.User.Avatar
  alias Wocky.User.BotEvent
  alias Wocky.User.Location
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

    has_many :bots, Bot
    has_many :tokens, Token
    has_many :devices, Device
    has_many :locations, Location
    has_many :bot_events, BotEvent
    has_many :conversations, Conversation
    has_many :tros_metadatas, TROSMetadata

    many_to_many :subscriptions, Bot, join_through: "bot_subscribers"
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

  @register_fields [:username, :server, :external_id, :phone_number,
                    :password, :pass_details]
  @update_fields [:handle, :avatar, :first_name, :last_name, :email]

  @doc """
  Creates a new user with a password.
  Used for testing only.
  """
  @spec register(username, server, binary, binary) :: {:ok, t} | {:error, any}
  def register(username, server, password, pass_details) do
    %{username: username,
      server: server,
      external_id: username,
      password: password,
      pass_details: pass_details}
    |> register_changeset()
    |> Repo.insert
  end

  @doc """
  Creates or updates a user based on the external authentication ID and
  phone number.
  """
  @spec register(server, external_id, phone_number) ::
    {:ok, {username, server, boolean}} | no_return
  def register(server, external_id, phone_number) do
    case Repo.get_by(User, external_id: external_id) do
      nil ->
        user =
          %{username: ID.new,
            server: server,
            external_id: external_id,
            phone_number: phone_number}
          |> register_changeset()
          |> Repo.insert!

        {:ok, {user.username, user.server, true}}

      user ->
        {:ok, {user.username, user.server, false}}
    end
  end

  def register_changeset(params) do
    %User{}
    |> cast(params, @register_fields)
    |> validate_required([:username, :server, :external_id])
    |> validate_format(:phone_number, ~r//) # TODO
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

  @doc "Subscribe to the bot"
  @spec subscribe_to_bot(t, Bot.t) :: :ok
  def subscribe_to_bot(user, bot) do
    user
    |> Repo.preload(:subscriptions)
    |> Ecto.Changeset.change()
    |> Ecto.Changeset.put_assoc(:subscriptions, [bot])
    |> Repo.update!

    :ok
  end

  @doc "Returns all bots that the user subscribes to"
  @spec get_subscribed_bots(t) :: [Bot.t]
  def get_subscribed_bots(user) do
    user = Repo.preload(user, :subscriptions)
    user.subscriptions
  end

  @doc "Returns all bots that the user owns"
  @spec get_owned_bots(t) :: [Bot.t]
  def get_owned_bots(user) do
    user
    |> Ecto.assoc(:bots)
    |> Repo.all
  end

  @doc "Returns all bots that the user owns and has set to 'follow me'"
  @spec get_owned_bots_with_follow_me(t) :: [Bot.t]
  def get_owned_bots_with_follow_me(user) do
    user
    |> Ecto.assoc(:bots)
    |> with_follow_me()
    |> Repo.all
  end

  defp with_follow_me(query) do
    from b in query,
      where: b.follow_me == ^true and b.follow_me_expiry > ^Timestamp.now
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

  def changeset(struct, params) do
    struct
    |> cast(params, @update_fields)
    |> validate_format(:email, ~r/@/)
    |> validate_change(:handle, &validate_handle/2)
    |> validate_change(:avatar, &validate_avatar(&1, struct, &2))
    |> unique_constraint(:handle)
    |> prepare_changes(fn changeset ->
      maybe_cleanup_avatar(changeset.changes[:avatar], struct.avatar)
      changeset
    end)
  end

  defp validate_handle(:handle, handle) do
    if Enum.member?(reserved_handles(), String.downcase(handle)) do
      [handle: "unavailable"]
    else
      []
    end
  end

  defp reserved_handles,
    do: Application.get_env(:wocky, :reserved_handles, [])

  defp validate_avatar(:avatar, user, avatar) do
    case do_validate_avatar(user, avatar) do
      {:ok, _} -> []
      {:error, :not_found} ->
        [avatar: "does not exist"]
      {:error, :invalid_url} ->
        [avatar: "not a valid TROS URL"]
      {:error, :not_local_file} ->
        [avatar: "not a local file"]
      {:error, :not_file_owner} ->
        [avatar: "not owned by the user"]
    end
  end

 defp do_validate_avatar(user, avatar) do
   Avatar.prepare(avatar)
   ~>> Avatar.check_is_local(user.server)
   ~>> Avatar.check_owner(user.id)
 end

 defp do_update_index(%User{username: username}, fields) do
    :ok = Index.user_updated(username, fields)
  end

  defp maybe_cleanup_avatar(new_avatar, old_avatar) do
    Avatar.maybe_delete_existing(new_avatar, old_avatar)
  end

  @spec set_location(t, resource, float, float, float) :: :ok | {:error, any}
  def set_location(user, resource, lat, lon, accuracy) do
    case Location.insert(user, resource, lat, lon, accuracy) do
      {:ok, loc} ->
        loc
        |> Location.check_for_bot_events
        |> Location.update_bot_locations

        :ok

      {:error, _} = error->
        error
    end
  end

  @doc "Removes the user from the database"
  @spec delete(id) :: :ok | no_return
  def delete(id) do
    User
    |> with_user(id)
    |> Repo.delete_all

    :ok = Index.user_removed(id)
  end

  @spec to_jid(t, binary | nil) :: JID.t
  def to_jid(%User{id: user, server: server} = u, resource \\ nil) do
    JID.make!(user, server, resource || (u.resource || ""))
  end
end
