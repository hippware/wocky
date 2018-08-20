defmodule WockyAPI.Callbacks.Notification do
  @moduledoc """
  Callbacks for notification changes
  """

  use Wocky.Watcher, type: Wocky.User.Notification, events: [:insert]

  alias Absinthe.Subscription
  alias WockyAPI.Endpoint
  alias WockyAPI.Resolvers.{Notification, User}

  def handle_insert(%Event{new: notification}) do
    topic = User.notification_subscription_topic(notification.user_id)

    graphql_notification = Notification.to_graphql(notification)

    Subscription.publish(Endpoint, graphql_notification, [
      {:notifications, topic}
    ])
  end
end
