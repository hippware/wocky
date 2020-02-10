# credo:disable-for-this-file Credo.Check.Readability.Specs
defmodule WockyAPI.Resolvers.Notification do
  @moduledoc "GraphQL resolver for bot objects"

  import WockyAPI.Resolvers.Utils

  alias Wocky.Notifier.InBand
  alias Wocky.Notifier.InBand.Notification

  @type_map [
    {:bot_invitation, :bot_invitation_notification},
    {:bot_invitation_response, :bot_invitation_response_notification},
    {:bot_item, :bot_item_notification},
    {:geofence_event, :geofence_event_notification},
    {:location_share, :location_share_notification},
    {:location_share_end, :location_share_end_notification},
    {:location_share_end_self, :location_share_end_self_notification},
    {:location_share_nearby_end, :location_share_nearby_end_notification},
    {:location_share_nearby_start, :location_share_nearby_start_notification},
    {:user_befriend, :user_befriend_notification},
    {:user_invitation, :user_invitation_notification}
  ]

  def to_graphql(
        %Notification{
          type: :location_share,
          share_type: share_type,
          other_user_share_type: other_user_share_type
        } = n
      ) do
    n
    |> Map.put(:share_types, %{to: other_user_share_type, from: share_type})
    |> common_to_graphql()
  end

  def to_graphql(%Notification{} = n), do: common_to_graphql(n)

  defp common_to_graphql(%{id: id, created_at: created_at} = n) do
    data = Map.put(n, :user, n.other_user)

    %{data: data, id: id, created_at: created_at}
  end

  # -------------------------------------------------------------------
  # Connections

  def resolve_type(%{type: type}, _), do: Keyword.fetch!(@type_map, type)

  def get_notifications(parent, args, %{context: %{current_user: requestor}}) do
    requestor
    |> InBand.user_query(
      args[:before_id],
      args[:after_id],
      map_types(args[:types])
    )
    |> connection_from_query(parent, args,
      postprocess: &Notification.populate_virtual_fields/1
    )
    |> map_edges(&to_graphql/1)
  end

  defp map_types(nil), do: nil

  defp map_types(types) do
    Enum.reduce(@type_map, [], fn {t, gqlt}, acc ->
      if Enum.member?(types, gqlt) do
        [t | acc]
      else
        acc
      end
    end)
  end

  defp map_edges({:ok, %{edges: edges} = result}, fun),
    do: {:ok, %{result | edges: Enum.map(edges, &update_in(&1[:node], fun))}}

  defp map_edges(result, _), do: result

  # -------------------------------------------------------------------
  # Mutations

  def notification_delete(args, %{context: %{current_user: requestor}}) do
    InBand.delete(args.input.id, requestor)

    {:ok, true}
  end

  # -------------------------------------------------------------------
  # Subscriptions

  def notification_subscription_topic(user_id),
    do: "notification_subscription_" <> user_id

  def resolve_update_type(%{data: _}, _), do: :notification

  def resolve_update_type(_, _), do: :notification_deleted
end
