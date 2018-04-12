defmodule WockyAPI.Resolvers.Bot do
  @moduledoc "GraphQL resolver for bot objects"

  alias Wocky.Bot
  alias Wocky.Bot.Item
  alias Wocky.GeoUtils
  alias Wocky.Repo
  alias Wocky.Repo.ID
  alias Wocky.User
  alias WockyAPI.Resolvers.Utils

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
    id
    |> Bot.get_query()
    |> visible_query(requestor)
    |> Utils.connection_from_query(user, args)

  end
  defp do_get_bots(user, requestor, %{relationship: relationship} = args) do
    user
    |> Bot.by_relationship_query(relationship)
    |> visible_query(requestor)
    |> Utils.connection_from_query(user, args)
  end
  defp do_get_bots(_user, _requestor, _args) do
    {:error, "Either 'id' or 'relationship' must be specified"}
  end

  def get_bot_relationships(%{parent: %User{} = user,
                              node: %Bot{} = bot}, _args, _info) do
    {:ok, User.get_bot_relationships(user, bot)}
  end
  def get_bot_relationships(%{parent: %Bot{} = bot,
                              node: %User{} = user}, _args, _info) do
    {:ok, User.get_bot_relationships(user, bot)}
  end

  def get_lat(bot, _args, _info) do
    {:ok, Bot.lat(bot)}
  end

  def get_lon(bot, _args, _info) do
    {:ok, Bot.lon(bot)}
  end

  def create_bot(_root, args, %{context: %{current_user: user}}) do
    args[:input][:values]
    |> parse_lat_lon()
    |> Map.put(:id, ID.new())
    |> Map.put(:user_id, user.id)
    |> Bot.insert()
  end

  def update_bot(_root, args, %{context: %{current_user: %{id: user_id}}}) do
    case Bot.get(args[:input][:id], true) do
      %Bot{user_id: ^user_id} = bot ->
        updates = parse_lat_lon(args[:input][:values])
        Bot.update(bot, updates)
      nil ->
        {:error, "Invalid bot"}
    end
  end

  defp parse_lat_lon(%{lat: lat, lon: lon} = input) do
    Map.put(input, :location, GeoUtils.point(lat, lon))
  end

  defp parse_lat_lon(input), do: input

  def get_items(bot, args, _info) do
    bot
    |> Item.items_query()
    |> Utils.connection_from_query(bot, args)
  end

  def get_subscribers(_root, %{id: _, type: _}, _info) do
    {:error, "Only one of 'id' or 'type' may be specified"}
  end
  def get_subscribers(bot, %{id: id} = args, _info) do
    bot
    |> Bot.subscriber_query(id)
    |> Utils.connection_from_query(bot, args)
  end
  def get_subscribers(bot, %{type: type} = args, _info) do
    subscribers_query =
      case type do
        :subscriber -> Bot.subscribers_query(bot)
        :guest -> Bot.guests_query(bot)
        :visitor -> Bot.visitors_query(bot)
      end

    subscribers_query
    |> Utils.connection_from_query(bot, args)
  end
  def get_subscribers(_root, _args, _info) do
    {:error, "At least one of 'id' or 'type' must be specified"}
  end

  def get_owner(object, _args, _info) do
    object = Repo.preload(object, :user)
    {:ok, object.user}
  end

  def subscribe(_root, args, %{context: %{current_user: user}}) do
    case Bot.get(args[:id]) do
      nil -> not_found_error(args[:id])
      bot ->
        Bot.subscribe(bot, user, args[:guest] || false)
        {:ok, %{result: true}}
    end
  end

  def unsubscribe(_root, args, %{context: %{current_user: user}}) do
    case Bot.get(args[:id]) do
      nil -> not_found_error(args[:id])
      bot ->
        Bot.unsubscribe(bot, user)
        {:ok, %{result: true}}
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
