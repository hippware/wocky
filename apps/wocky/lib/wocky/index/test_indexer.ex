defmodule Wocky.Index.TestIndexer do
  @moduledoc """
  Implements a testing `Indexer`. All index changes are recorded and the
  call is logged, but no further action occurs. The index can then be
  queried by the testing system.
  """

  alias Wocky.Bot
  alias Wocky.Repo

  require Logger

  @behaviour Wocky.Index

  def init do
    Logger.info("Test indexer enabled")

    if :ets.info(:test_index_table) == :undefined do
      _ = :ets.new(:test_index_table, [:public, :named_table, :duplicate_bag])
    end

    reset()
  end

  def update_object(index, id, map) do
    :ets.insert(:test_index_table, {id, index, :update, map})
    :ok
  end

  def delete_object(index, id) do
    :ets.insert(:test_index_table, {id, index, :delete, nil})
    :ok
  end

  def geosearch(_index, lat, lon) do
    # Do a brute-force search of the bots
    loc = %{lat: lat, lon: lon}

    {:ok, bots} =
      Repo.transaction(fn ->
        Bot
        |> Repo.stream()
        |> Stream.filter(&Bot.contains?(&1, loc))
        |> Stream.map(&bot_to_object(&1, loc))
        |> Enum.to_list()
      end)

    {:ok, bots}
  end

  defp bot_to_object(bot, loc) do
    distance = Bot.distance_from(bot, loc)

    %{
      "objectID" => bot.id,
      "server" => bot.server,
      "user_id" => bot.user_id,
      "title" => bot.title,
      "image" => bot.image,
      "lat" => Bot.lat(bot),
      "lon" => Bot.lon(bot),
      "radius" => bot.radius,
      "public" => bot.public,
      # meters
      "_rankingInfo" => %{"geoDistance" => distance / 1000}
    }
  end

  def get_index_operations do
    :ets.tab2list(:test_index_table)
  end

  def reset do
    :ets.delete_all_objects(:test_index_table)
    :ok
  end
end
