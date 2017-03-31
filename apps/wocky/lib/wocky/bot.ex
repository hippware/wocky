defmodule Wocky.Bot do
  @moduledoc ""

  use Wocky.JID

  alias Wocky.Index
  alias Wocky.User.Location
  alias __MODULE__, as: Bot

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

  @type t :: %Bot{
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

  use ExConstructor

  @spec to_jid(Bot.t) :: Ejabberd.jid
  def to_jid(bot) do
    :wocky_bot_util.make_jid(bot.server, bot.id)
  end

  @spec to_jid_string(Bot.t) :: binary
  def to_jid_string(bot) do
    bot |> to_jid |> JID.encode
  end

  @spec get(binary) :: nil | Bot.t
  def get(id) do
    case :wocky_db_bot.get_bot(<<>>, id) do
      :not_found -> nil
      bot -> new(bot)
    end
  end

  @spec set_location(Bot.t, Location.t) :: :ok
  def set_location(%{id: id} = _bot, %{lat: lat, lon: lon} = _location) do
    Schemata.update :bot, in: :wocky_db.shared_keyspace,
      set: %{lat: lat, lon: lon},
      where: %{id: id}

    Index.bot_updated(id, %{lat: lat, lon: lon})
  end

  @spec insert(Bot.t) :: :ok
  def insert(%Bot{} = struct) do
    bot = Map.from_struct(struct)
    :wocky_db_bot.insert("", bot)
  end
end
