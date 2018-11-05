defmodule WockyAPI.Resolvers.Bot do
  @moduledoc "GraphQL resolver for bot objects"

  alias Absinthe.Subscription
  alias Wocky.{Bot, Repo, User, Waiter}
  alias Wocky.Bot.{Invitation, Item}
  alias Wocky.GeoUtils
  alias Wocky.Repo.ID
  alias Wocky.User.Location
  alias WockyAPI.Endpoint
  alias WockyAPI.Resolvers.Utils

  def get_bot(_root, args, %{context: context}) do
    case Map.get(context, :current_user) do
      nil -> {:ok, nil}
      user -> {:ok, Bot.get_bot(args[:id], user)}
    end
  end

  def get_bots(%User{} = user, args, %{context: %{current_user: requestor}}) do
    do_get_bots(user, requestor, args)
  end

  def get_local_bots(_root, args, %{context: %{current_user: requestor}}) do
    point_a = GeoUtils.point(args[:point_a][:lat], args[:point_a][:lon])
    point_b = GeoUtils.point(args[:point_b][:lat], args[:point_b][:lon])

    bots =
      requestor
      |> Bot.by_relationship_query(:subscribed, requestor)
      |> Bot.filter_by_location(point_a, point_b)
      |> Repo.all()

    {:ok, bots}
  end

  defp do_get_bots(_user, _requestor, %{id: _, relationship: _}) do
    {:error, "Only one of 'id' or 'relationship' may be specified"}
  end

  defp do_get_bots(user, requestor, %{id: id} = args) do
    id
    |> Bot.get_bot_query(requestor)
    |> Utils.connection_from_query(user, args)
  end

  defp do_get_bots(user, requestor, %{relationship: relationship} = args) do
    user
    |> Bot.by_relationship_query(relationship, requestor)
    |> Utils.connection_from_query(user, args)
  end

  defp do_get_bots(_user, _requestor, _args) do
    {:error, "Either 'id' or 'relationship' must be specified"}
  end

  def get_bot_relationships(
        %{parent: %User{} = user, node: %Bot{} = bot},
        _args,
        _info
      ) do
    {:ok, User.get_bot_relationships(user, bot)}
  end

  def get_bot_relationships(
        %{parent: %Bot{} = bot, node: %User{} = user},
        _args,
        _info
      ) do
    {:ok, User.get_bot_relationships(user, bot)}
  end

  def get_lat(bot, _args, _info) do
    {:ok, Bot.lat(bot)}
  end

  def get_lon(bot, _args, _info) do
    {:ok, Bot.lon(bot)}
  end

  def get_active_bots(_root, args, %{context: %{current_user: user}}) do
    user
    |> Bot.active_bots_query()
    |> Utils.connection_from_query(user, args)
  end

  def create_bot(_root, %{input: input}, %{context: %{current_user: user}}) do
    with {:ok, bot} <-
           input[:values]
           |> parse_lat_lon()
           |> Map.put(:id, ID.new())
           |> Map.put(:user_id, user.id)
           |> Bot.insert(),
         {:ok, _} <- maybe_update_location(input, user, bot) do
      {:ok, bot}
    end
  end

  def create_bot(_root, %{}, %{context: %{current_user: user}}),
    do: {:ok, Bot.preallocate(user.id)}

  def update_bot(_root, %{input: input}, %{context: %{current_user: requestor}}) do
    case Bot.get_owned_bot(input[:id], requestor, true) do
      nil ->
        not_found_error(input[:id])

      bot ->
        updates = parse_lat_lon(input[:values])

        with {:ok, bot} <- Bot.update(bot, updates),
             {:ok, _} <- maybe_update_location(input, requestor, bot) do
          {:ok, bot}
        end
    end
  end

  defp maybe_update_location(%{user_location: l}, user, bot)
       when not is_nil(l) do
    bot
    |> Bot.sub_setup_event()
    |> Waiter.wait(2000, fn ->
      Enum.member?(User.get_bot_relationships(user, bot), :subscribed)
    end)

    location = struct(Location, l)

    User.set_location_for_bot(user, location, bot)
  end

  defp maybe_update_location(_, _, _), do: {:ok, :skip}

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
        :guest -> Bot.subscribers_query(bot)
        :visitor -> Bot.visitors_query(bot)
      end

    subscribers_query
    |> Utils.connection_from_query(bot, args)
  end

  def get_subscribers(_root, _args, _info) do
    {:error, "At least one of 'id' or 'type' must be specified"}
  end

  def subscribe(_root, %{input: input}, %{context: %{current_user: requestor}}) do
    case Bot.get_bot(input[:id], requestor) do
      nil ->
        not_found_error(input[:id])

      bot ->
        Bot.subscribe(bot, requestor)

        with {:ok, _} <- maybe_update_location(input, requestor, bot) do
          {:ok, true}
        end
    end
  end

  def unsubscribe(_root, %{input: %{id: bot_id}}, %{
        context: %{current_user: requestor}
      }) do
    case Bot.get_bot(bot_id, requestor) do
      nil ->
        not_found_error(bot_id)

      bot ->
        Bot.unsubscribe(bot, requestor)
        {:ok, true}
    end
  end

  def delete(_root, %{input: %{id: bot_id}}, %{
        context: %{current_user: %{id: user_id} = requestor}
      }) do
    case Bot.get_bot(bot_id, requestor) do
      nil ->
        not_found_error(bot_id)

      bot = %{user_id: ^user_id} ->
        Bot.delete(bot)
        {:ok, true}

      _ ->
        not_owned_error()
    end
  end

  def visitor_subscription_topic(user_id) do
    "visitor_subscription_" <> user_id
  end

  def notify_visitor_subscription(bot, subscriber, entered) do
    to_notify = bot |> Bot.subscribers_query() |> Repo.all()

    action =
      case entered do
        true -> :arrive
        false -> :depart
      end

    notification = %{bot: bot, visitor: subscriber, action: action}

    targets =
      Enum.map(
        to_notify,
        &{:bot_guest_visitors, visitor_subscription_topic(&1.id)}
      )

    Subscription.publish(Endpoint, notification, targets)
  end

  def publish_item(_root, args, %{context: %{current_user: requestor}}) do
    values = args[:input][:values]

    with %Bot{} = bot <- Bot.get_bot(args[:input][:bot_id], requestor),
         {:ok, item} <- Item.put(values[:id], bot, requestor, values[:stanza]) do
      {:ok, item}
    else
      nil -> not_found_error(args[:input][:bot_id])
      {:error, :permission_denied} -> {:error, "Permission denied"}
    end
  end

  def delete_item(_root, args, %{context: %{current_user: requestor}}) do
    with %Bot{} = bot <- Bot.get_bot(args[:input][:bot_id], requestor),
         :ok <- Item.delete(args[:input][:id], bot, requestor) do
      {:ok, true}
    else
      nil -> not_found_error(args[:input][:bot_id])
      {:error, :permission_denied} -> {:error, "Permission denied"}
      {:error, :not_found} -> {:error, "Item not found"}
      error -> error
    end
  end

  def invite(_root, args, %{context: %{current_user: requestor}}) do
    with %Bot{} = bot <- Bot.get_owned_bot(args[:input][:bot_id], requestor) do
      {:ok, Enum.map(args[:input][:user_ids], &do_invite(&1, bot, requestor))}
    else
      nil -> {:error, "Invalid bot"}
    end
  end

  defp do_invite(invitee, bot, requestor) do
    with %User{} = invitee <- User.get_user(invitee, requestor),
         {:ok, invitation} <- Invitation.put(invitee, bot, requestor) do
      invitation
    else
      nil -> {:error, "Invalid user"}
      {:error, :permission_denied} -> {:error, "Permission denied"}
      error -> error
    end
  end

  def invitation_respond(
        _root,
        %{input: %{invitation_id: id, accept: accept?} = input},
        %{context: %{current_user: requestor}}
      ) do
    with %Invitation{} = invitation <- Invitation.get(id, requestor),
         {:ok, result} <- Invitation.respond(invitation, accept?, requestor),
         {:ok, _} <- maybe_update_location(input, requestor, result.bot) do
      {:ok, true}
    else
      nil -> {:error, "Invalid invitation"}
      {:error, :permission_denied} -> {:error, "Permission denied"}
      error -> error
    end
  end

  defp not_found_error(id), do: {:error, "Bot not found: #{id}"}
  defp not_owned_error, do: {:error, "Operation only permitted on owned bots"}
end
