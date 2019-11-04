defmodule WockyAPI.Resolvers.Notification do
  @moduledoc "GraphQL resolver for bot objects"

  alias Wocky.Notifier.InBand.Notification
  alias WockyAPI.Resolvers.Utils

  @type_map [
    {:bot_invitation, :bot_invitation_notification},
    {:bot_invitation_response, :bot_invitation_response_notification},
    {:bot_item, :bot_item_notification},
    {:geofence_event, :geofence_event_notification},
    {:location_share, :location_share_notification},
    {:location_share_end, :location_share_end_notification},
    {:location_share_end_self, :location_share_end_self_notification},
    {:user_invitation, :user_invitation_notification},
    {:user_proximity, :user_proximity_notification}
  ]

  def to_graphql(%Notification{id: id, created_at: created_at} = n),
    do: %{data: n, id: id, created_at: created_at}

  # -------------------------------------------------------------------
  # Connections

  def resolve_type(%{type: type}, _), do: Keyword.fetch!(@type_map, type)

  def get_notifications(parent, args, %{context: %{current_user: requestor}}) do
    requestor
    |> Notification.user_query(
      args[:before_id],
      args[:after_id],
      map_types(args[:types])
    )
    |> Utils.connection_from_query(parent, args)
    |> Utils.map_edges(&to_graphql/1)
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

  # -------------------------------------------------------------------
  # Mutations

  def notification_delete(args, %{context: %{current_user: requestor}}) do
    Notification.delete(args.input.id, requestor)

    {:ok, true}
  end

  # -------------------------------------------------------------------
  # Subscriptions

  def notification_subscription_topic(user_id),
    do: "notification_subscription_" <> user_id

  def resolve_update_type(%{data: _}, _), do: :notification

  def resolve_update_type(_, _), do: :notification_deleted
end
