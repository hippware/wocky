defmodule Wocky.User do
  @moduledoc ""

  use Exref, ignore: [insert: 1, new: 2, from_jid: 3, to_jid: 2,
                      to_jid_string: 2, to_bare_jid: 1, to_bare_jid_string: 1]
  use Wocky.Ejabberd

  import OK, only: ["~>>": 2]

  alias Wocky.Bot
  alias Wocky.ID
  alias Wocky.Index
  alias Wocky.Location
  alias Wocky.Repo
  alias Wocky.Repo.Doc
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
    :phone_number
  ]

  @type t :: %User{
    id:             binary,
    server:         binary,
    resource:       nil | binary,
    handle:         nil | binary,
    avatar:         nil | binary,
    first_name:     nil | binary,
    last_name:      nil | binary,
    email:          nil | binary,
    external_id:    nil | binary,
    phone_number:   nil | binary
  }

  @type id           :: binary
  @type server       :: binary
  @type resource     :: binary
  @type handle       :: binary
  @type external_id  :: binary
  @type phone_number :: binary
  @type password     :: binary
  @type search_key   :: :external_id | :phone_number | :handle

  @bucket_type "users"

  # ====================================================================
  # API
  # ====================================================================

  @doc "Create a user object"
  @spec new(id, server, map | list) :: t
  def new(id, server, data \\ %{}) do
    struct(%User{id: id, server: server}, data)
  end

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
    old_user = maybe_lookup_user(id, server, fields)

    fields
    |>  Enum.filter(&filter_map/1)
    |>  Enum.into(%{id: id, server: server})
    |>  check_reserved_handle()
    ~>> check_duplicate_handle(old_user.handle)
    ~>> prepare_avatar()
    ~>> delete_existing_avatar(old_user.avatar)
    ~>> do_update_user()
  end

  defp maybe_lookup_user(id, server, fields)
  do
    if fields[:avatar] || fields[:handle] do
      find(id, server) || new(id, server)
    else
      new(id, server)
    end
  end

  defp filter_map({:resource, _}), do: false
  defp filter_map({_, nil}), do: false
  defp filter_map(_), do: true

  defp check_reserved_handle(%{handle: handle} = fields)
  when not is_nil(handle)
  do
    reserved = Application.get_env(:wocky, :reserved_handles, [])
    if Enum.member?(reserved, String.downcase(handle)) do
      {:error, :duplicate_handle}
    else
      {:ok, fields}
    end
  end
  defp check_reserved_handle(fields), do: {:ok, fields}

  # WARNING: There is plenty of room for a race condition here
  # FIXME: We need a better/more reliable way to detect multiple handles
  defp check_duplicate_handle(%{handle: handle} = fields, old_handle)
  when not is_nil(handle) and handle != old_handle
  do
    case search(:handle, handle) do
      [] -> {:ok, fields}
      _ -> {:error, :duplicate_handle}
    end
  end
  defp check_duplicate_handle(fields, _), do: {:ok, fields}

  defp prepare_avatar(%{avatar: avatar} = fields)
  when not is_nil(avatar)
  do
    result =
      :tros.parse_url(avatar)
      ~>> check_file_is_local(fields.server)
      ~>> check_avatar_owner(fields.id)

    case result do
      :ok -> {:ok, fields}
      error -> error
    end
  end
  defp prepare_avatar(fields), do: {:ok, fields}

  defp check_file_is_local({server, _} = data, server), do: {:ok, data}
  defp check_file_is_local(_, _), do: {:error, :not_local_file}

  defp check_avatar_owner({server, id}, user_id) do
    case :tros.get_metadata(server, id) ~>> :tros.get_owner do
      {:ok, ^user_id} -> :ok
      {:ok, _} -> {:error, :not_file_owner}
      error -> error
    end
  end

  defp delete_existing_avatar(%{avatar: new_avatar} = fields, old_avatar)
  when not is_nil(new_avatar)
    and not is_nil(old_avatar)
    and new_avatar != old_avatar
  do
    case :tros.parse_url(old_avatar) do
      {:ok, {file_server, file_id}} ->
        :tros.delete(file_server, file_id)

      {:error, _} -> :ok
    end

    {:ok, fields}
  end
  defp delete_existing_avatar(data, _), do: {:ok, data}

  defp do_update_user(%{id: id, server: server} = fields) do
    :ok = Repo.update(fields, @bucket_type, server, id)
    :ok = Index.user_updated(id, fields)
    :ok
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

  @spec set_location(t, Location.t) :: t
  def set_location(user, location) do
    :ok = :wocky_db_user.set_location(user.id, user.server, user.resource,
                                      location.lat, location.lon,
                                      location.accuracy)
    user
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
