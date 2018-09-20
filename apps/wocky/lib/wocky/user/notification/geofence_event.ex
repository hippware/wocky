defmodule Wocky.User.Notification.GeofenceEvent do
  @moduledoc """
  Notification for a guest entering or exiting a bot to which the notified
  user is subscribed
  """

  import EctoHomoiconicEnum, only: [defenum: 2]

  defenum GeofenceEventType, [:enter, :exit]

  defstruct [
    :user_id,
    :other_user_id,
    :bot_id,
    :event
  ]

  @type t :: %__MODULE__{}
end

defimpl Wocky.User.Notifier, for: Wocky.User.Notification.GeofenceEvent do
  alias Wocky.User.Notification

  def notify(geofence_event) do
    geofence_event
    |> Map.put(:geofence_event, geofence_event.event)
    |> Notification.put(
      :geofence_event,
      [:user_id, :other_user_id, :bot_id, :geofence_event]
    )
  end

  def decode(geofence_event, params) do
    %{
      geofence_event
      | user_id: params.user_id,
        other_user_id: params.other_user_id,
        bot_id: params.bot_id,
        event: params.geofence_event
    }
  end
end
