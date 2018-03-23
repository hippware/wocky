defmodule WockyAPI.BotResolver do
  @moduledoc "GraphQL resolver for bot objects"

  alias Wocky.Bot

  def get_public_bot(_root, args, _info) do
    case Bot.get(args[:id]) do
      bot = %Bot{public: true} -> {:ok, bot}
      _ -> {:error, "Bot not found: " <> args[:id]}
    end
  end

  def get_lat(_root, _args, info) do
    {:ok, Bot.lat(info.source)}
  end

  def get_lon(_root, _args, info) do
    {:ok, Bot.lon(info.source)}
  end
end
