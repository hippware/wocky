defmodule WockyAPI.Callbacks.Notification do
  @moduledoc """
  Callbacks for notification changes
  """

  use Wocky.Watcher, type: Wocky.User.Notification, events: [:insert, :delete]

  alias Absinthe.Subscription
  alias WockyAPI.Endpoint
  alias WockyAPI.Resolvers.{Notification, User}

  def handle_insert(%Event{new: notification}) do
    notification
    |> Notification.to_graphql()
    |> publish(notification.user_id)
  end

  def handle_delete(%Event{old: notification}) do
    %{id: notification.id}
    |> publish(notification.user_id)
  end

  defp publish(data, user_id) do
    topic = User.notification_subscription_topic(user_id)

    Subscription.publish(Endpoint, data, [{:notifications, topic}])
  end
end
