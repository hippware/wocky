# credo:disable-for-this-file Credo.Check.Readability.Specs
defmodule WockyAPI.Resolvers.Bot do
  @moduledoc "GraphQL resolver for bot objects"

  import WockyAPI.Resolvers.Utils

  alias Wocky.Account
  alias Wocky.Account.User
  alias Wocky.GeoUtils
  alias Wocky.Location
  alias Wocky.Location.UserLocation
  alias Wocky.POI
  alias Wocky.POI.Bot
  alias Wocky.Relation
  alias Wocky.Relation.Invitation
  alias Wocky.Repo.ID
  alias Wocky.Waiter

  @max_local_bots 50
  @default_local_bots 15

  def default_local_bots, do: @default_local_bots

  def max_local_bots, do: @max_local_bots

  defdelegate max_local_bots_search_radius, to: Relation

  # -------------------------------------------------------------------
  # Connections

  def get_bots(%User{} = user, args, %{context: %{current_user: requestor}}) do
    do_get_bots(user, requestor, args)
  end

  defp do_get_bots(_user, _requestor, %{id: _, relationship: _}) do
    {:error, "Only one of 'id' or 'relationship' may be specified"}
  end

  defp do_get_bots(user, requestor, %{id: id} = args) do
    id
    |> Relation.get_bot_query(requestor)
    |> connection_from_query(user, args)
  end

  defp do_get_bots(user, requestor, %{relationship: relationship} = args) do
    user
    |> Relation.by_relationship_query(relationship, requestor)
    |> connection_from_query(user, args)
  end

  defp do_get_bots(_user, _requestor, _args) do
    {:error, "Either 'id' or 'relationship' must be specified"}
  end

  def get_active_bots(args, %{context: %{current_user: user}}) do
    user
    |> Relation.active_bots_query()
    |> connection_from_query(user, args)
  end

  def get_items(bot, args, _info) do
    bot
    |> POI.get_items_query()
    |> connection_from_query(bot, args)
  end

  def get_subscribers(_root, %{id: _, type: _}, _info) do
    {:error, "Only one of 'id' or 'type' may be specified"}
  end

  def get_subscribers(bot, %{id: id} = args, _info) do
    bot
    |> Relation.subscriber_query(id)
    |> connection_from_query(bot, args)
  end

  def get_subscribers(bot, %{type: type} = args, _info) do
    subscribers_query =
      case type do
        :subscriber -> Relation.subscribers_query(bot)
        :visitor -> Relation.visitors_query(bot)
      end

    subscribers_query
    |> connection_from_query(bot, args)
  end

  def get_subscribers(_root, _args, _info) do
    {:error, "At least one of 'id' or 'type' must be specified"}
  end

  def get_bot_relationships(
        %{parent: %User{} = user, node: %Bot{} = bot},
        _args,
        _info
      ) do
    {:ok, Relation.get_bot_relationships(user, bot)}
  end

  def get_bot_relationships(
        %{parent: %Bot{} = bot, node: %User{} = user},
        _args,
        _info
      ) do
    {:ok, Relation.get_bot_relationships(user, bot)}
  end

  # -------------------------------------------------------------------
  # Queries

  def get_bot(args, %{context: %{current_user: requestor}}) do
    {:ok, Relation.get_bot(args[:id], requestor)}
  end

  def get_local_bots(%{limit: n}, _info) when n > @max_local_bots,
    do: {:error, "Maximum local bots is #{@max_local_bots}"}

  def get_local_bots(args, %{context: %{current_user: requestor}}) do
    point_a = map_point(args[:point_a])
    point_b = map_point(args[:point_b])
    limit = args[:limit] || @default_local_bots

    case Relation.get_local_bots(requestor, point_a, point_b, limit) do
      {:ok, bots} ->
        {:ok, %{bots: bots, area_too_large: false}}

      {:error, :area_too_large} ->
        {:ok, %{bots: [], area_too_large: true}}
    end
  end

  def get_local_bots_cluster(args, %{context: %{current_user: requestor}}) do
    point_a = map_point(args[:point_a])
    point_b = map_point(args[:point_b])

    case Relation.get_local_bots_clustered(
           requestor,
           point_a,
           point_b,
           args[:lat_divs],
           args[:lon_divs]
         ) do
      {:ok, bots, clusters} ->
        {:ok, %{bots: bots, clusters: clusters, area_too_large: false}}

      {:error, :area_too_large} ->
        {:ok, %{bots: [], clusters: [], area_too_large: true}}
    end
  end

  defp map_point(point_arg) do
    {lat, lon} = GeoUtils.normalize_lat_lon(point_arg[:lat], point_arg[:lon])
    GeoUtils.point(lat, lon)
  end

  def get_lat(%{location: l}, _args, _info) do
    {:ok, GeoUtils.get_lat(l)}
  end

  def get_lon(%{location: l}, _args, _info) do
    {:ok, GeoUtils.get_lon(l)}
  end

  # -------------------------------------------------------------------
  # Mutations

  def bot_create(%{input: input}, %{context: %{current_user: user}}) do
    with {:ok, bot} <-
           input[:values]
           |> parse_lat_lon()
           |> Map.put(:id, ID.new())
           |> Map.put(:user_id, user.id)
           |> POI.insert(user),
         {:ok, _} <- maybe_update_location(input, user, bot) do
      {:ok, bot}
    end
  end

  def bot_create(%{}, %{context: %{current_user: user}}),
    do: POI.preallocate(user)

  def bot_update(%{input: input}, %{context: %{current_user: requestor}}) do
    case Relation.get_owned_bot(input[:id], requestor, true) do
      nil ->
        not_found_error(input[:id])

      bot ->
        updates = parse_lat_lon(input[:values])

        with {:ok, bot} <- POI.update(bot, updates),
             {:ok, _} <- maybe_update_location(input, requestor, bot) do
          {:ok, bot}
        end
    end
  end

  defp maybe_update_location(%{user_location: l}, user, bot)
       when not is_nil(l) do
    bot
    |> POI.sub_setup_event()
    |> Waiter.wait(5000, fn -> Relation.subscribed?(user, bot) end)

    Location.set_user_location_for_bot(user, UserLocation.new(l), bot)
  end

  defp maybe_update_location(_, _, _), do: {:ok, :skip}

  defp parse_lat_lon(%{lat: lat, lon: lon} = input) do
    Map.put(input, :location, GeoUtils.point(lat, lon))
  end

  defp parse_lat_lon(input), do: input

  def bot_delete(%{input: %{id: bot_id}}, %{
        context: %{current_user: %{id: user_id} = requestor}
      }) do
    case Relation.get_bot(bot_id, requestor) do
      nil ->
        not_found_error(bot_id)

      bot = %{user_id: ^user_id} ->
        POI.delete(bot)
        {:ok, true}

      _ ->
        not_owned_error()
    end
  end

  def bot_subscribe(%{input: input}, %{context: %{current_user: requestor}}) do
    case Relation.get_bot(input[:id], requestor) do
      nil ->
        not_found_error(input[:id])

      bot ->
        with :ok <- Relation.subscribe(requestor, bot),
             {:ok, _} <- maybe_update_location(input, requestor, bot) do
          {:ok, true}
        else
          {:error, :permission_denied} -> {:error, "Permission denied"}
          error -> error
        end
    end
  end

  def bot_unsubscribe(%{input: %{id: bot_id}}, %{
        context: %{current_user: requestor}
      }) do
    case Relation.get_bot(bot_id, requestor) do
      nil ->
        not_found_error(bot_id)

      bot ->
        :ok = Relation.unsubscribe(requestor, bot)
        {:ok, true}
    end
  end

  def bot_item_publish(%{input: args}, %{context: %{current_user: requestor}}) do
    id = args[:id]
    content = args[:content]
    image_url = args[:image_url]

    with %Bot{} = bot <- Relation.get_bot(args.bot_id, requestor),
         {:ok, item} <- POI.put_item(bot, id, content, image_url, requestor) do
      {:ok, item}
    else
      nil -> not_found_error(args.bot_id)
      {:error, :permission_denied} -> {:error, "Permission denied"}
      error -> error
    end
  end

  def bot_item_delete(args, %{context: %{current_user: requestor}}) do
    with %Bot{} = bot <- Relation.get_bot(args[:input][:bot_id], requestor),
         :ok <- POI.delete_item(bot, args[:input][:id], requestor) do
      {:ok, true}
    else
      nil -> not_found_error(args[:input][:bot_id])
      {:error, :permission_denied} -> {:error, "Permission denied"}
      {:error, :not_found} -> {:error, "Item not found"}
      error -> error
    end
  end

  def bot_invite(args, %{context: %{current_user: requestor}}) do
    case Relation.get_owned_bot(args[:input][:bot_id], requestor) do
      nil ->
        {:error, "Invalid bot"}

      bot ->
        {:ok, Enum.map(args[:input][:user_ids], &do_invite(&1, bot, requestor))}
    end
  end

  defp do_invite(invitee, bot, requestor) do
    with %User{} = invitee <- Account.get_user(invitee, requestor),
         {:ok, invitation} <- Relation.invite(invitee, bot, requestor) do
      invitation
    else
      nil -> {:error, "Invalid user"}
      {:error, :permission_denied} -> {:error, "Permission denied"}
      error -> error
    end
  end

  def bot_invitation_respond(
        %{input: %{invitation_id: id, accept: accept?} = input},
        %{context: %{current_user: requestor}}
      ) do
    with %Invitation{} = invitation <- Relation.get_invitation(id, requestor),
         {:ok, result} <- Relation.respond(invitation, accept?, requestor),
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

  # -------------------------------------------------------------------
  # Subscriptions

  def visitor_subscription_topic(user_id) do
    "visitor_subscription_" <> user_id
  end

  def notify_visitor_subscription(bot, subscriber, entered, updated_at) do
    to_notify = Relation.get_subscribers(bot)

    action =
      case entered do
        true -> :arrive
        false -> :depart
      end

    notification = %{
      bot: bot,
      visitor: subscriber,
      action: action,
      updated_at: updated_at
    }

    to_notify
    |> Enum.map(&visitor_subscription_topic(&1.id))
    |> publish_subscription(:bot_guest_visitors, notification)
  end
end
