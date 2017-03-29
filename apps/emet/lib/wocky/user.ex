defmodule Wocky.User do
  @moduledoc ""

  use Wocky.Ejabberd

  import OK, only: ["~>>": 2]

  alias Wocky.Bot
  alias Wocky.ID
  alias Wocky.Index
  alias Wocky.Repo
  alias Wocky.Repo.Doc
  alias Wocky.User.Avatar
  alias Wocky.User.Handle
  alias Wocky.User.Location
  alias Wocky.User.Token
  alias __MODULE__, as: User

  @enforce_keys [:id, :server]
  defstruct [
    :id,
    :server,
    :resource,
    :handle,
    :avatar,
    :first_name,
    :last_name,
    :email,
    :external_id,
    :phone_number,
    :location
  ]

  @type id           :: binary
  @type server       :: binary
  @type resource     :: binary
  @type external_id  :: binary
  @type phone_number :: binary
  @type search_key   :: :external_id | :phone_number | :handle

  @type t :: %User{
    id:             id,
    server:         server,
    resource:       nil | resource,
    handle:         nil | Handle.t,
    avatar:         nil | Avatar.url,
    first_name:     nil | binary,
    last_name:      nil | binary,
    email:          nil | binary,
    external_id:    nil | external_id,
    phone_number:   nil | phone_number,
    location:       nil | map
  }

  @bucket_type "users"

  @doc "Create a user object"
  @spec new(id, server, map | list) :: t
  def new(id, server, data \\ %{}) do
    struct(%User{id: id, server: server}, data)
  end

  # ====================================================================
  # Persistence API

  @doc """
  Creates or updates a user based on the external authentication ID and
  phone number.
  """
  @spec register(server, external_id, phone_number) ::
    {:ok, {id, server, boolean}}
  def register(server, external_id, phone_number) do
    case search(:external_id, external_id) do
      [] ->
        id = ID.new
        :ok = update(id, server, %{
                       external_id: external_id,
                       phone_number: phone_number
                     })
        :ok = wait_for_user(id)
        {:ok, {id, server, true}}

      [user] ->
        {:ok, {user.id, user.server, false}}
    end
  end

  @doc "Wait for a new user to be indexed for searching"
  @spec wait_for_user(id, pos_integer, pos_integer) :: :ok | {:error, :timeout}
  def wait_for_user(id, sleep_time \\ 250, retries \\ 10) do
    Repo.wait_for_search_result(@bucket_type, "id_register:#{id}",
                                sleep_time, retries)
  end

  @spec insert(t) :: :ok
  def insert(%User{id: id, server: server} = user) do
    insert(id, server, Map.from_struct(user))
  end

  @spec insert(id, server, map) :: :ok
  def insert(id, server, fields) do
    fields
    |> Enum.filter(&filter_map/1)
    |> Enum.into(%{id: id, server: server})
    |> do_insert_user()
    |> do_update_index()
  end

  defp filter_map({:resource, _}), do: false
  defp filter_map({_, nil}), do: false
  defp filter_map(_), do: true

  defp do_insert_user(%{id: id, server: server} = fields) do
    :ok = Repo.update(fields, @bucket_type, server, id)
    fields
  end

  defp do_update_index(%{id: id} = fields) do
    :ok = Index.user_updated(id, fields)
  end

  @spec update(t) :: :ok | {:error, term}
  def update(%User{id: id, server: server} = user) do
    update(id, server, Map.from_struct(user))
  end

  @doc """
  Update the data on an existing user.
  Fields is a map containing fields to update.
  """
  @spec update(id, server, map) :: :ok | {:error, term}
  def update(id, server, fields) do
    new_handle = fields[:handle]
    new_avatar = fields[:avatar]

    fields
    |>  prepare_user(id, server)
    |>  set_handle(new_handle)
    ~>> set_avatar(new_avatar)
    ~>> insert
  end

  defp prepare_user(fields, id, server) do
    old_user = if fields[:avatar] || fields[:handle] do
      find(id, server)
    end

    new_user = new(id, server, fields)
    if old_user do
      %User{new_user | avatar: old_user.avatar, handle: old_user.handle}
    else
      new_user
    end
  end

  @doc "Removes the user from the database"
  @spec delete(id, server) :: :ok
  def delete(id, server) do
    :ok = Token.release_all(id, server)
    :ok = Repo.delete(@bucket_type, server, id)
    :ok = Index.user_removed(id)
    :ok
  end

  @doc """
  Returns a map of all fields for a given user or `nil' if no such
  user exists.
  """
  @spec find(binary, binary) :: t | nil
  def find(id, server) do
    case Repo.find(@bucket_type, server, id) do
      nil -> nil
      data -> new(id, server, data)
    end
  end

  @doc "Search for a user based on the value of a property."
  @spec search(search_key, binary) :: [t]
  def search(field, value) do
    @bucket_type
    |> Repo.search("#{field}_register:\"#{value}\"")
    |> Enum.map(&Doc.to_map/1)
    |> Enum.map(fn data -> new(data[:id], data[:server], data) end)
  end

  # ====================================================================
  # Convenience API

  @spec get_handle(t | nil) :: Handle.t
  def get_handle(user) do
    case user && user.handle do
      nil -> ""
      handle -> handle
    end
  end

  @doc """
  Sets the handle member on the passed in user struct if the handle passes
  all validations. The new handle must be unique and not one of the reserved
  handles.
  """
  @spec set_handle(t, Handle.t) :: {:ok, t} | {:error, :duplicate_handle}
  def set_handle(%User{handle: handle} = user, handle), do: {:ok, user}
  def set_handle(user, nil), do: {:ok, user}
  def set_handle(user, handle) do
    Handle.check_reserved(handle)
    ~>> Handle.check_duplicate
    ~>> do_set_handle(user)
  end

  defp do_set_handle(handle, user) do
    {:ok, %User{user | handle: handle}}
  end

  @spec get_avatar(t | nil) :: binary | nil
  def get_avatar(user) do
    user && user.avatar
  end

  @spec set_avatar(t, binary) :: {:ok, t} | {:error, any}
  def set_avatar(%User{avatar: avatar} = user, avatar), do: {:ok, user}
  def set_avatar(user, nil), do: {:ok, user}
  def set_avatar(user, avatar) do
    Avatar.prepare(avatar)
    ~>> Avatar.check_is_local(user.server)
    ~>> Avatar.check_owner(user.id)
    ~>> Avatar.delete_existing(user.avatar)
    ~>> Avatar.to_url
    ~>> do_set_avatar(user)
  end

  defp do_set_avatar(avatar, user) do
    {:ok, %User{user | avatar: avatar}}
  end

  @spec get_location(t | nil) :: Location.t | nil
  def get_location(user) do
    user && user.location
  end

  @spec set_location(t, Location.t) :: {:ok, t}
  def set_location(user, location) do
    do_set_location(user, location)
    ~>> Location.check_for_bot_events(location)
    ~>> Location.update_bot_locations(location)
  end

  defp do_set_location(user, location) do
    %User{user | location: Map.from_struct(location)}
  end

  # =========================================================================

  @spec to_jid(t, binary | nil) :: Ejabberd.jid
  def to_jid(%User{id: user, server: server} = u, resource \\ nil) do
    Ejabberd.make_jid!(user, server, resource || (u.resource || ""))
  end

  @spec to_jid_string(t, binary | nil) :: binary
  def to_jid_string(%User{} = user, resource \\ nil) do
    user |> to_jid(resource) |> :jid.to_binary
  end

  @spec to_bare_jid(t) :: Ejabberd.jid
  def to_bare_jid(%User{} = user) do
    user |> to_jid |> :jid.to_bare
  end

  @spec to_bare_jid_string(t) :: binary
  def to_bare_jid_string(%User{} = user) do
    user |> to_bare_jid |> :jid.to_binary
  end

  @spec from_jid(binary, binary, binary) :: t
  def from_jid(user, server, resource) do
    %User{id: user, server: server, resource: resource}
  end

  @spec from_jid(Ejabberd.jid) :: t
  def from_jid(jid) do
    jid(user: user, server: server, resource: resource) = jid
    from_jid(user, server, resource)
  end

  @spec get_subscribed_bots(t) :: [Ejabberd.jid]
  def get_subscribed_bots(user) do
    :wocky_db_bot.subscribed_bots(to_jid(user))
  end

  @spec get_owned_bots(t) :: [Bot.t]
  def get_owned_bots(user) do
    :all
    |> Schemata.select(
        from: :user_bot, in: :wocky_db.shared_keyspace,
        where: %{owner: to_bare_jid_string(user)})
    |> Enum.map(&Bot.new(&1))
  end

  @spec get_last_bot_event(t, binary) :: [map]
  def get_last_bot_event(user, bot_id) do
    Schemata.select :all,
      from: :bot_event, in: :wocky_db.local_keyspace,
      where: %{jid: to_jid_string(user), bot: bot_id},
      limit: 1
  end

  @spec add_bot_event(t, binary, :enter | :exit) :: boolean
  def add_bot_event(user, bot_id, event) do
    Schemata.insert into: :bot_event, in: :wocky_db.local_keyspace,
      values: %{
        jid: to_jid_string(user),
        bot: bot_id,
        event: event,
        created_at: :now
      }
  end
end
