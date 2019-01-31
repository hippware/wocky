defmodule WockyAPI.Resolvers.Notification do
  @moduledoc "GraphQL resolver for bot objects"

  alias Wocky.User.Notification
  alias WockyAPI.Resolvers.Utils

  @type_map [
    {:bot_item, :bot_item_notification},
    {:geofence_event, :geofence_event_notification},
    {:bot_invitation, :bot_invitation_notification},
    {:bot_invitation_response, :bot_invitation_response_notification},
    {:user_invitation, :user_invitation_notification}
  ]

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

  def to_graphql(%Notification{id: id, created_at: created_at} = n),
    do: %{data: n, id: id, created_at: created_at}

  def resolve_type(%{type: type}, _), do: Keyword.fetch!(@type_map, type)

  def resolve_update_type(%{data: _}, _), do: :notification

  def resolve_update_type(_, _), do: :notification_deleted

  defp map_types(nil), do: Enum.map(@type_map, &elem(&1, 0))

  defp map_types(types) do
    Enum.reduce(@type_map, [], fn {t, gqlt}, acc ->
      if Enum.member?(types, gqlt) do
        [t | acc]
      else
        acc
      end
    end)
  end
end
