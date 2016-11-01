defmodule Wocky.User do
  @moduledoc ""

  use Wocky.Ejabberd
  alias :wocky_db, as: Db

  @type t :: %__MODULE__{
    user:           binary,
    server:         binary,
    resource:       binary,
    handle:         binary,
    password:       binary,
    avatar:         binary,
    first_name:     binary,
    last_name:      binary,
    email:          binary,
    external_id:    binary,
    phone_number:   binary,
    roster_viewers: [binary]
  }

  @enforce_keys [:user, :server]
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

  def make_id do
    Db.create_id
  end

  def to_jid(user, resource \\ <<>>) do
    :jid.make(user.user, user.server, resource)
  end

  def to_jid_string(user, resource \\ <<>>) do
    :jid.to_binary(to_jid(user, resource))
  end

  def from_jid(user, server, resource) do
    %__MODULE__{user: user, server: server, resource: resource}
  end

  def from_jid(jid) do
    jid(user: user, server: server, resource: resource) = jid
    from_jid(user, server, resource)
  end

  def set_location(user, location) do
    :ok = :wocky_db_user.set_location(user.user, user.server, user.resource,
                                      location.lat, location.lon,
                                      location.accuracy)
    user
  end

  def get_followed_bots(user) do
    :wocky_db_bot.followed_bots(user.server, to_jid(user))
  end

  def get_last_bot_event(user, bot_id) do
    Schemata.select :all,
      from: :bot_event, in: :wocky_db.local_keyspace,
      where: %{jid: to_jid_string(user), bot: bot_id},
      limit: 1
  end

  def add_bot_event(user, bot_id, event) do
    Schemata.insert into: :bot_event, in: :wocky_db.local_keyspace,
      values: %{
        jid: to_jid_string(user),
        bot: bot_id,
        event: event,
        created_at: :now
      }
  end

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
