defmodule WockyAPI.Schema.NotificationTypes do
  @moduledoc """
  Absinthe types for wocky notification list
  """

  use WockyAPI.Schema.Notation

  alias WockyAPI.Resolvers.Notification

  # -------------------------------------------------------------------
  # Objects

  enum :notification_type do
    @desc "BotInvitationNotification type"
    value :bot_invitation_notification

    @desc "BotInvitationResponseNotification type"
    value :bot_invitation_response_notification

    @desc "BotItemNotification type"
    value :bot_item_notification

    @desc "GeofenceEventNotification type"
    value :geofence_event_notification

    @desc "LocationShareNotification type"
    value :location_share_notification

    @desc "LocationShareEndNotification type"
    value :location_share_end_notification

    @desc "LocationShareEndSelfNotification type"
    value :location_share_end_self_notification

    @desc "LocationShareNearbyStart type"
    value :location_share_nearby_start_notification

    @desc "UserInvitationNotification type"
    value :user_invitation_notification

    @desc "UserProximityNotification type"
    value :user_proximity_notification
  end

  union :notification_data do
    types [
      :bot_invitation_notification,
      :bot_invitation_response_notification,
      :bot_item_notification,
      :geofence_event_notification,
      :location_share_notification,
      :location_share_end_notification,
      :location_share_end_self_notification,
      :location_share_nearby_start_notification,
      :user_invitation_notification,
      :user_proximity_notification
    ]

    resolve_type &Notification.resolve_type/2
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

  # NOTE We aren't using dataloader for the user field in these notifications.
  # In order to avoid a race condition that shows up in testing, we are
  # preloading the user when the event is generated.

  @desc "A notification that a user has invited the recipient to a bot"
  object :bot_invitation_notification do
    @desc "The invitation object itself"
    field :invitation, non_null(:bot_invitation),
      resolve: dataloader(Wocky, :bot_invitation)

    @desc "The sender of the invitation"
    field :user, non_null(:user)

    @desc "The bot to which the recipient was invited"
    field :bot, non_null(:bot), resolve: dataloader(Wocky)
  end

  @desc """
  A notification that a user has responded to an invitation from the recipient
  """
  object :bot_invitation_response_notification do
    @desc "The invitation object"
    field :invitation, non_null(:bot_invitation),
      resolve: dataloader(Wocky, :bot_invitation)

    @desc "The user who replied to the invitation"
    field :user, non_null(:user)

    @desc "The bot to which the user was invited"
    field :bot, non_null(:bot), resolve: dataloader(Wocky)

    @desc "Whether the invitation was accepted or not"
    field :accepted, non_null(:boolean),
      resolve: fn n, _, _ -> {:ok, n.bot_invitation_accepted} end
  end

  @desc "A notification for posting or updating a bot item"
  object :bot_item_notification do
    @desc "The user who made the change"
    field :user, non_null(:user)

    @desc "The bot to which the item belongs"
    field :bot, non_null(:bot), resolve: dataloader(Wocky)

    @desc "The bot item that has been posted or edited"
    field :bot_item, non_null(:bot_item), resolve: dataloader(Wocky)
  end

  enum :geofence_event do
    @desc "A user has entered a bot"
    value :enter

    @desc "A user has exited a bot"
    value :exit
  end

  @desc "A notification that a user has entered or exited a subscribed bot"
  object :geofence_event_notification do
    @desc "The user who entered or exited"
    field :user, non_null(:user)

    @desc "The bot that was entered or exited"
    field :bot, non_null(:bot), resolve: dataloader(Wocky)

    @desc "The action that occurred"
    field :event, :geofence_event,
      resolve: fn n, _, _ -> {:ok, n.geofence_event} end
  end

  @desc """
  A notification that a user has begun sharing their location with the recipient
  """
  object :location_share_notification do
    @desc "The user sharing their location"
    field :user, non_null(:user)

    @desc "The ID of this share"
    field :share_id, :aint

    @desc "When the share is scheduled to automatically expire"
    field :expires_at, :datetime
  end

  @desc """
  A notification that a user has stopped sharing their location with the
  recipient
  """
  object :location_share_end_notification do
    @desc "The user sharing their location"
    field :user, non_null(:user)

    @desc "The ID of the ending share"
    field :share_id, :aint
  end

  @desc """
  A notification that a user has stopped sharing their location with the
  recipient
  """
  object :location_share_end_self_notification do
    @desc "The user to whom the recipient was sharing their location"
    field :user, non_null(:user)

    @desc "The ID of the ending share"
    field :share_id, :aint
  end

  object :location_share_nearby_start_notification do
    @desc """
    The user has moved within nearby range and has begun sharing their location.
    This will be sent at most once per nearby cooldown period.
    """
    field :user, non_null(:user)
  end

  @desc """
  A notification that a user has invited the receipied to be their friend
  """
  object :user_invitation_notification do
    @desc "The user who sent the invitation"
    field :user, non_null(:user)

    @desc "The share type of the user sending the invitation to the recipient"
    field :share_type, :friend_share_type
  end

  @desc """
  A notification that a user is within range of a proximity notification
  """
  object :user_proximity_notification do
    @desc "The user who is within range"
    field :user, non_null(:user)
  end

  # -------------------------------------------------------------------
  # Connections

  connection :notifications, node_type: :notification do
    total_count_field()

    edge do
    end
  end

  # -------------------------------------------------------------------
  # Queries

  object :notification_queries do
    @desc "Get the notifications for the current user"
    connection field :notifications, node_type: :notifications do
      @desc "ID which all results should be newer than"
      arg :after_id, :aint
      @desc "ID which all results should be older than"
      arg :before_id, :aint

      @desc """
      Filter for types of notification to retrieve. If null, all types
      will be included
      """
      arg :types, list_of(:notification_type)

      connection_complexity()
      resolve &Notification.get_notifications/3
    end
  end

  # -------------------------------------------------------------------
  # Mutations

  input_object :notification_delete_input do
    @desc "The id of the notification to delete"
    field :id, non_null(:aint)
  end

  payload_object(:notification_delete_payload, :boolean)

  object :notification_mutations do
    @desc """
    Delete an existing notification. If the notification does not exist
    then the request will succeed but no action will be taken
    """
    field :notification_delete, type: :notification_delete_payload do
      arg :input, non_null(:notification_delete_input)
      resolve &Notification.notification_delete/2
      middleware &build_payload/2
    end
  end

  # -------------------------------------------------------------------
  # Subscriptions

  @desc "Deletion of a notification"
  object :notification_deleted do
    @desc "The id of the deleted notification"
    field :id, non_null(:aint)
  end

  union :notification_update do
    types [
      :notification,
      :notification_deleted
    ]

    resolve_type &Notification.resolve_update_type/2
  end

  object :notification_subscriptions do
    @desc "Subscribe to newly created notifications for a user"
    field :notifications, non_null(:notification_update) do
      user_subscription_config(&Notification.notification_subscription_topic/1)
    end
  end
end
