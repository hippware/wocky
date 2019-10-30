defmodule WockyAPI.Callbacks.Notification do
  @moduledoc """
  Callbacks for notification changes
  """

  use DawdleDB.Handler, type: Wocky.Notifier.InBand.Notification

  alias Absinthe.Subscription
  alias WockyAPI.Endpoint
  alias WockyAPI.Resolvers.Notification

  def handle_insert(notification) do
    notification
    |> Notification.to_graphql()
    |> publish(notification.user_id)
  end

  def handle_delete(notification) do
    %{id: notification.id}
    |> publish(notification.user_id)
  end

  defp publish(data, user_id) do
    topic = Notification.notification_subscription_topic(user_id)

    Subscription.publish(Endpoint, data, [{:notifications, topic}])
  end
end
