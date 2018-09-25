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

  @desc "A single notification"
  object :notification do
    @desc "The notification's unique ID"
    field :id, :aint

    @desc "The type-specific data for the notification"
    field :data, non_null(:notification_data)

    @desc "The creation time of the notificaiton"
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

  union :notification_update do
    types [
      :notification,
      :notification_deleted
    ]

    resolve_type &Notification.resolve_update_type/2
  end

  @desc "Deletion of a notification"
  object :notification_deleted do
    @desc "The id of the deleted notification"
    field :id, non_null(:aint)
  end

  @desc "A notification for posting or updating a bot item"
  object :bot_item_notification do
    @desc "The user who made the change"
    field :user, non_null(:user), resolve: assoc(:other_user)

    @desc "The bot to which the item belongs"
    field :bot, non_null(:bot), resolve: assoc(:bot)

    @desc "The bot item that has been posted or edited"
    field :bot_item, non_null(:bot_item), resolve: assoc(:bot_item)
  end

  @desc "A notification that a user has entered or exited a subscribed bot"
  object :geofence_event_notification do
    @desc "The user who entered or exited"
    field :user, non_null(:user), resolve: assoc(:other_user)

    @desc "The bot that was entered or exited"
    field :bot, non_null(:bot), resolve: assoc(:bot)

    @desc "The action that occurred"
    field :event, :geofence_event,
      resolve: fn n, _, _ -> {:ok, n.geofence_event} end
  end

  @desc "A notification that a user has invited the recipient to a bot"
  object :invitation_notification do
    @desc "The invitation object itself"
    field :invitation, non_null(:bot_invitation), resolve: assoc(:invitation)

    @desc "The sender of the invitation"
    field :user, non_null(:user), resolve: assoc(:other_user)

    @desc "The bot to which the recipient was invited"
    field :bot, non_null(:bot), resolve: assoc(:bot)
  end

  @desc """
  A notification that a user has responded to an invitation from the recipient
  """
  object :invitation_response_notification do
    @desc "The invitation object"
    field :invitation, non_null(:bot_invitation), resolve: assoc(:invitation)

    @desc "The user who replied to the invitation"
    field :user, non_null(:user), resolve: assoc(:other_user)

    @desc "The bot to which the user was invited"
    field :bot, non_null(:bot), resolve: assoc(:bot)

    @desc "Whether the invitation was accepted or not"
    field :accepted, non_null(:boolean),
      resolve: fn n, _, _ -> {:ok, n.invitation_accepted} end
  end

  @desc "A notification that a user has started following the recipient"
  object :user_follow_notification do
    @desc "The user who started following"
    field :user, non_null(:user), resolve: assoc(:other_user)
  end

  enum :geofence_event do
    @desc "A user has entered a bot"
    value :enter

    @desc "A user has exited a bot"
    value :exit
  end

  object :notification_queries do
    @desc "Get the notifications for the current user"
    connection field :notifications, node_type: :notifications do
      @desc "ID which all results should be newer than"
      arg :after_id, :aint
      @desc "ID which all results should be older than"
      arg :before_id, :aint

      connection_complexity
      resolve &Notification.get_notifications/3
    end
  end

  object :notification_subscriptions do
    @desc "Subscribe to newly created notifications for a user"
    field :notifications, non_null(:notification_update) do
      user_subscription_config(&User.notification_subscription_topic/1)
    end
  end
end
