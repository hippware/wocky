defmodule Wocky.User do
  @moduledoc ""

  use Exref, ignore: [insert: 1, new: 2, from_jid: 3, to_jid: 2,
                      to_jid_string: 2, to_bare_jid: 1, to_bare_jid_string: 1]
  use Wocky.Ejabberd
  alias :wocky_db, as: Db
  alias Wocky.ID
  alias Wocky.Repo

  @type id           :: binary
  @type server       :: binary
  @type handle       :: binary
  @type external_id  :: binary
  @type phone_number :: binary
  @type password     :: binary
  @type token        :: binary

  @type t :: %__MODULE__{
    user:           binary,
    server:         binary,
    resource:       binary,
    handle:         nil | binary,
    password:       nil | binary,
    avatar:         nil | binary,
    first_name:     nil | binary,
    last_name:      nil | binary,
    email:          nil | binary,
    external_id:    nil | binary,
    phone_number:   nil | binary,
    roster_viewers: [binary]
  }

  defstruct [
    user:           nil,
    server:         nil,
    resource:       nil,
    handle:         nil,
    password:       nil,
    avatar:         nil,
    first_name:     nil,
    last_name:      nil,
    email:          nil,
    external_id:    nil,
    phone_number:   nil,
    roster_viewers: []
  ]

  use ExConstructor

  # ====================================================================
  # API
  # ====================================================================

  @doc """
  Creates or updates a user based on the external authentication ID and
  phone number.
  """
  @spec register(server, external_id, phone_number) ::
    {:ok, {id, server, boolean}}
  def register(server, external_id, phone_number) do
    case find_by_external_id(external_id) do
      nil ->
        id = ID.create
        :ok = insert(id, server, %{
                       external_id: external_id,
                       phone_number: phone_number
                     })
        {:ok, {id, server, true}}

      obj ->
        {:ok, {obj[:id_register], obj[:server_register], false}}
    end
  end

  def find_by_external_id(external_id) do
    Repo.search("users", "external_id_register:#{external_id}")
  end

  def insert(id, server, fields, wait \\ false) do
    :ok =
      fields
      |> Map.merge(%{id: id, server: server})
      |> Repo.update("users", server, id)

    maybe_wait_for_user(wait, id)
  end

  defp maybe_wait_for_user(wait, id, sleep_time \\ 250, retries \\ 10)
  defp maybe_wait_for_user(false, _, _, _), do: :ok
  defp maybe_wait_for_user(_, _, _, 0),     do: {:error, :user_not_found}
  defp maybe_wait_for_user(_, id, sleep_time, retries) do
    case Repo.search("users", "id_register:#{id}") do
      nil ->
        Process.sleep(sleep_time)
        maybe_wait_for_user(true, id, sleep_time, retries - 1)

      _ojb ->
        :ok
    end
  end

  @doc "Removes the user from the database"
  @spec delete(server, id) :: :ok
  def delete(server, id) do
    Repo.delete("users", server, id)
  end

  # =========================================================================

  @spec to_jid(Wocky.User.t, binary | nil) :: Ejabberd.jid
  def to_jid(%__MODULE__{user: user, server: server} = u, resource \\ nil) do
    Ejabberd.make_jid!(user, server, resource || (u.resource || ""))
  end

  @spec to_jid_string(Wocky.User.t, binary | nil) :: binary
  def to_jid_string(%__MODULE__{} = user, resource \\ nil) do
    user |> to_jid(resource) |> :jid.to_binary
  end

  @spec to_bare_jid(Wocky.User.t) :: Ejabberd.jid
  def to_bare_jid(%__MODULE__{} = user) do
    user |> to_jid |> :jid.to_bare
  end

  @spec to_bare_jid_string(Wocky.User.t) :: binary
  def to_bare_jid_string(%__MODULE__{} = user) do
    user |> to_bare_jid |> :jid.to_binary
  end

  @spec from_jid(binary, binary, binary) :: Wocky.User.t
  def from_jid(user, server, resource) do
    %__MODULE__{user: user, server: server, resource: resource}
  end

  @spec from_jid(Ejabberd.jid) :: Wocky.User.t
  def from_jid(jid) do
    jid(user: user, server: server, resource: resource) = jid
    from_jid(user, server, resource)
  end

  @spec set_location(Wocky.User.t, Wocky.Location.t) :: Wocky.User.t
  def set_location(user, location) do
    :ok = :wocky_db_user.set_location(user.user, user.server, user.resource,
                                      location.lat, location.lon,
                                      location.accuracy)
    user
  end

  @spec get(binary) :: Wocky.User.t | nil
  def get(user_id) do
    :all
    |> Schemata.select(
        from: :user, in: :wocky_db.shared_keyspace,
        where: %{user: user_id, server: :wocky_app.server})
    |> handle_user_return
  end

  defp handle_user_return([]), do: nil
  defp handle_user_return([user]) do
    user
    |> Enum.reject(fn {_, v} -> is_nil(v) end)
    |> Wocky.User.new
  end

  @spec get_subscribed_bots(Wocky.User.t) :: [Ejabberd.jid]
  def get_subscribed_bots(user) do
    :wocky_db_bot.subscribed_bots(to_jid(user))
  end

  @spec get_owned_bots(Wocky.User.t) :: [Wocky.Bot.t]
  def get_owned_bots(user) do
    :all
    |> Schemata.select(
        from: :user_bot, in: :wocky_db.shared_keyspace,
        where: %{owner: to_bare_jid_string(user)})
    |> Enum.map(&Wocky.Bot.new(&1))
  end

  @spec get_last_bot_event(Wocky.User.t, binary) :: [map]
  def get_last_bot_event(user, bot_id) do
    Schemata.select :all,
      from: :bot_event, in: :wocky_db.local_keyspace,
      where: %{jid: to_jid_string(user), bot: bot_id},
      limit: 1
  end

  @spec add_bot_event(Wocky.User.t, binary, :enter | :exit) :: boolean
  def add_bot_event(user, bot_id, event) do
    Schemata.insert into: :bot_event, in: :wocky_db.local_keyspace,
      values: %{
        jid: to_jid_string(user),
        bot: bot_id,
        event: event,
        created_at: :now
      }
  end

  @spec insert(Wocky.User.t) :: :ok
  def insert(%__MODULE__{} = struct) do
    {phone_number, user} =
      struct
      |> Map.from_struct
      |> Map.delete(:resource)
      |> Map.pop(:phone_number)

    :ok = Db.insert(:shared, :user, user)
    :ok = maybe_insert_phone_lookup(user, phone_number)
    :ok = maybe_insert_handle_lookup(user)
  end

  defp maybe_insert_phone_lookup(_user, nil), do: :ok
  defp maybe_insert_phone_lookup(user, phone_number) do
    Db.insert(:shared, :phone_number_to_user,
              %{user: user.user, server: user.server,
                phone_number: phone_number})
  end

  defp maybe_insert_handle_lookup(%{handle: nil}), do: :ok
  defp maybe_insert_handle_lookup(user) do
    Db.insert(:shared, :handle_to_user,
              %{user: user.user, server: user.server, handle: user.handle})
  end
end
