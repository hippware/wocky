defmodule WockyAPI.BotResolver do
  @moduledoc "GraphQL resolver for bot objects"

  import Ecto.Query

  alias Absinthe.Relay.Connection
  alias Wocky.Bot
  alias Wocky.Bot.Item
  alias Wocky.Bot.Subscription
  alias Wocky.GeoUtils
  alias Wocky.Repo
  alias Wocky.Repo.ID
  alias WockyAPI.UtilResolver

  def get_public_bot(_root, args, _info) do
    case Bot.get(args[:id]) do
      bot = %Bot{public: true} -> {:ok, bot}
      _ -> {:error, "Bot not found: " <> args[:id]}
    end
  end

  def get_bot(_root, args, %{context: %{current_user: _}}) do
    {:ok, Bot.get(args[:id])}
  end

  def get_lat(_root, _args, info) do
    {:ok, Bot.lat(info.source)}
  end

  def get_lon(_root, _args, info) do
    {:ok, Bot.lon(info.source)}
  end

  def insert_bot(_root, args, %{context: %{current_user: user}}) do
    input = parse_lat_lon(args[:bot])

    case args[:id] do
      nil ->
        input
        |> Map.put(:id, ID.new())
        |> Map.put(:user_id, user.id)
        |> Bot.insert()

      id ->
        update_bot(id, input, user)
    end
  end

  defp update_bot(id, input, %{id: user_id}) do
    case Bot.get(id, true) do
      %Bot{user_id: ^user_id} = bot -> Bot.update(bot, input)
      nil -> {:error, "Invalid bot"}
    end
  end

  defp parse_lat_lon(%{lat: lat, lon: lon} = input) do
    Map.put(input, :location, GeoUtils.point(lat, lon))
  end

  defp parse_lat_lon(input), do: input

  def get_items(_root, args, %{source: bot}) do
    query = Item.items_query(bot)

    query
    |> order_by(asc: :updated_at)
    |> Connection.from_query(&Repo.all/1, args)
    |> UtilResolver.add_query(query)
  end

  def get_subscribers(_root, args, %{source: bot}) do
    subscriber_query =
      case args[:type] do
        :subscriber -> Subscription.subscribers_query(bot)
        :guest -> Subscription.guests_query(bot)
        :visitor -> Subscription.visitors_query(bot)
      end

    subscriber_query
    |> order_by(asc: :updated_at)
    |> preload(:user)
    |> Connection.from_query(&Repo.all/1, args)
    |> UtilResolver.extract_nodes(:user, :subscription)
    |> UtilResolver.add_query(subscriber_query)
    |> UtilResolver.add_data(:bot, bot)
  end

  def get_subscription_type(_root, _args, %{source: %{subscription: sub}}) do
    type =
      cond do
        sub.visitor -> :visitor
        sub.guest -> :guest
        true -> :subscriber
      end

    {:ok, type}
  end
end
