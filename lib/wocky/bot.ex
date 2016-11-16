defmodule Wocky.Bot do
  @moduledoc ""

  use Exref, ignore: [insert: 1]
  alias :wocky_db, as: Db

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

  @spec get(binary) :: nil | map
  def get(id) do
    case :wocky_db_bot.get(<<>>, id) do
      :not_found -> nil
      bot -> new(bot)
    end
  end

  @spec set_location(Wocky.Bot.t, Wocky.Location.t) :: :ok
  def set_location(bot, location) do
    Schemata.update :bot, in: :wocky_db.shared_keyspace,
      set: %{lat: location.lat, lon: location.lon},
      where: %{id: bot.id}
  end

  @spec insert(Wocky.Bot.t) :: :ok
  def insert(%__MODULE__{} = struct) do
    bot = struct |> Map.from_struct

    :ok = Db.insert(:shared, :bot, bot)
  end
end
