defmodule WockyAPI.BotResolver do
  @moduledoc "GraphQL resolver for bot objects"

  alias Absinthe.Relay.Connection
  alias Wocky.Bot
  alias Wocky.GeoUtils

  def get_public_bot(_root, args, _info) do
    case Bot.get(args[:id]) do
      bot = %Bot{public: true} -> {:ok, bot}
      _ -> {:error, "Bot not found: " <> args[:id]}
    end
  end

  def get_lat(_root, _args, info) do
    lat = info.source.location |> GeoUtils.get_lat_lon() |> elem(0)
    {:ok, lat}
  end

  def get_lon(_root, _args, info) do
    lon = info.source.location |> GeoUtils.get_lat_lon() |> elem(1)
    {:ok, lon}
  end
end
