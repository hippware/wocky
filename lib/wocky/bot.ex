defmodule Wocky.Bot do
  @moduledoc ""

  use Exref, ignore: [insert: 1, new: 1, new: 2, to_jid: 1]
  use Wocky.Ejabberd

  @type t :: %__MODULE__{
    id:               binary,
    server:           binary,
    title:            binary,
    shortname:        binary,
    owner:            binary,
    description:      binary,
    image:            binary,
    type:             binary,
    address:          binary,
    lat:              float,
    lon:              float,
    radius:           integer,
    visibility:       integer,
    affiliates:       [binary],
    alerts:           integer,
    updated:          integer,
    follow_me:        boolean,
    follow_me_expiry: integer
  }

  defstruct [
    id:               nil,
    server:           nil,
    title:            nil,
    shortname:        nil,
    owner:            nil,
    description:      nil,
    image:            nil,
    type:             nil,
    address:          nil,
    lat:              nil,
    lon:              nil,
    radius:           nil,
    visibility:       nil,
    affiliates:       [],
    alerts:           nil,
    updated:          nil,
    follow_me:        nil,
    follow_me_expiry: nil
  ]

  use ExConstructor

  @spec make_id :: binary
  def make_id do
    :wocky_db.create_id
  end

  @spec to_jid(Wocky.Bot.t) :: Ejabberd.jid
  def to_jid(bot) do
    :wocky_bot_util.make_jid(bot.server, bot.id)
  end

  @spec to_jid_string(Wocky.Bot.t) :: binary
  def to_jid_string(bot) do
    bot |> to_jid |> :jid.to_binary
  end

  @spec get(binary) :: nil | map
  def get(id) do
    case :wocky_db_bot.get_bot(<<>>, id) do
      :not_found -> nil
      bot -> new(bot)
    end
  end

  @spec set_location(Wocky.Bot.t, Wocky.Location.t) :: :ok
  def set_location(%{id: id} = _bot, %{lat: lat, lon: lon} = _location) do
    Schemata.update :bot, in: :wocky_db.shared_keyspace,
      set: %{lat: lat, lon: lon},
      where: %{id: id}

    :wocky_index.bot_updated(%{id: id, lat: lat, lon: lon})
  end

  @spec insert(Wocky.Bot.t) :: :ok
  def insert(%__MODULE__{} = struct) do
    bot = struct |> Map.from_struct
    :wocky_db_bot.insert("", bot)
  end
end
