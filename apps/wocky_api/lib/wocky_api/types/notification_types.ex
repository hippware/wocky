defmodule WockyAPI.Schema.NotificationTypes do
  @moduledoc """
  Absinthe types for wocky notification list
  """

  use WockyAPI.Schema.Notation
  use Absinthe.Ecto, repo: Wocky.Repo

  alias WockyAPI.Resolvers.Notification
  alias WockyAPI.Resolvers.User

  connection :notifications, node_type: :notification do
    total_count_field

    edge do
    end
  end

  object :notification do
    field :id, :aint
    field :data, non_null(:notification_data)
    field :created_at, non_null(:datetime)
  end

  union :notification_data do
    types [
      :bot_item_notification,
      :geofence_event_notification,
      :invitation_notification,
      :invitation_response_notification,
      :user_follow_notification
    ]

    resolve_type &Notification.resolve_type/2
  end

  object :bot_item_notification do
    field :user, non_null(:user), resolve: assoc(:other_user)
    field :bot, non_null(:bot), resolve: assoc(:bot)
    field :bot_item, non_null(:bot_item), resolve: assoc(:bot_item)
  end

  object :geofence_event_notification do
    field :user, non_null(:user), resolve: assoc(:other_user)
    field :bot, non_null(:bot), resolve: assoc(:bot)

    field :event, :geofence_event,
      resolve: fn n, _, _ -> {:ok, n.geofence_event} end
  end

  object :invitation_notification do
    field :invitation, non_null(:bot_invitation), resolve: assoc(:invitation)
    field :user, non_null(:user), resolve: assoc(:other_user)
    field :bot, non_null(:bot), resolve: assoc(:bot)
  end

  object :invitation_response_notification do
    field :invitation, non_null(:bot_invitation), resolve: assoc(:invitation)
    field :user, non_null(:user), resolve: assoc(:other_user)
    field :bot, non_null(:bot), resolve: assoc(:bot)

    field :accepted, non_null(:boolean),
      resolve: fn n, _, _ -> {:ok, n.invitation_accepted} end
  end

  object :user_follow_notification do
    field :user, non_null(:user), resolve: assoc(:other_user)
  end

  enum :geofence_event do
    value :enter
    value :exit
  end

  object :notification_queries do
    connection field :notifications, node_type: :notifications do
      connection_complexity
      resolve &Notification.get_notifications/3
    end
  end

  object :notification_subscriptions do
    field :notifications, non_null(:notification) do
      user_subscription_config(&User.notification_subscription_topic/1)
    end
  end
end
