defmodule Wocky.Bot do
  @moduledoc ""

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
    updated:          integer
  }

  @enforce_keys [:id, :server, :owner]
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
    updated:          nil
  ]

  def make_id do
    :ossp_uuid.make(:v1, :binary)
  end

  def get(id) do
    case :wocky_db_bot.get(<<>>, id) do
      :not_found -> nil
      bot -> bot
    end
  end

  def insert(%__MODULE__{} = struct) do
    bot = struct |> Map.from_struct

    :ok = Db.insert(:shared, :bot, bot)
  end
end
