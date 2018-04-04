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
  alias Wocky.User
  alias WockyAPI.UtilResolver

  def get_bot(_root, args, %{context: context}) do
    {:ok,
      args[:id]
      |> Bot.get_query()
      |> visible_query(Map.get(context, :current_user))
      |> Repo.one()
    }
  end

  def get_bots(%User{} = user, args, %{context: %{current_user: requestor}}) do
    do_get_bots(user, requestor, args)
  end
  def get_bots(_root, args, %{context: %{current_user: user}}) do
    do_get_bots(user, user, args)
  end
  def get_bots(%User{} = user, args, _info) do
    do_get_bots(user, nil, args)
  end

  defp do_get_bots(_user, _requestor, %{id: _, relationship: _}) do
    {:error, "Only one of 'id' or 'relationship' may be specified"}
  end
  defp do_get_bots(user, requestor, %{id: id} = args) do
    query =
      id
      |> Bot.get_query()
      |> visible_query(requestor)

    query
    |> order_by(asc: :updated_at)
    |> Connection.from_query(&Repo.all/1, args)
    |> UtilResolver.add_query(query)
    |> UtilResolver.add_edge_parent(user)
  end
  defp do_get_bots(user, requestor, %{relationship: relationship} = args) do
    query =
      user
      |> Bot.by_relationship_query(relationship)
      |> visible_query(requestor)

    query
    |> order_by(asc: :updated_at)
    |> Connection.from_query(&Repo.all/1, args)
    |> UtilResolver.add_query(query)
    |> UtilResolver.add_edge_parent(user)
  end
  defp do_get_bots(_user, _requestor, _args) do
    {:error, "Either 'id' or 'relationship' must be specified"}
  end

  def get_bot_relationships(source, _args, _info) do
    {:ok, User.get_bot_relationships(source.parent, source.node)}
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

  def subscribe(_root, args, %{context: %{current_user: user}}) do
    case Bot.get(args[:id]) do
      nil -> not_found_error(args[:id])
      bot ->
        Bot.subscribe(bot, user, args[:guest] || false)
        {:ok, true}
    end
  end

  def unsubscribe(_root, args, %{context: %{current_user: user}}) do
    case Bot.get(args[:id]) do
      nil -> not_found_error(args[:id])
      bot ->
        Bot.unsubscribe(bot, user)
        {:ok, true}
    end
  end

  defp not_found_error(id), do: {:error, "Bot not found: #{id}"}

  defp visible_query(query, nil) do
    query
    |> Bot.is_public_query()
  end
  defp visible_query(query, user) do
    query
    |> Bot.is_visible_query(user)
  end
end
