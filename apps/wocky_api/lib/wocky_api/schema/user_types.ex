defmodule WockyAPI.Schema.UserTypes do
  @moduledoc """
  Absinthe types for wocky user
  """

  use WockyAPI.Schema.Notation

  import Absinthe.Resolution.Helpers
  import Kronky.Payload

  alias WockyAPI.Resolvers.{
    Block,
    Bot,
    Media,
    Message,
    User
  }

  @desc "The main Wocky user interface"
  interface :user do
    @desc "The user's unique ID"
    field :id, non_null(:uuid), do: scope(:public)

    @desc "The user's unique handle"
    field :handle, :string, do: scope(:public)

    @desc "The user's avatar"
    field :media, :media do
      resolve &Media.get_media/3
    end

    @desc "The user's first name"
    field :first_name,
          :string,
          deprecate: "Please use the single 'name' field" do
      resolve &User.get_first_name/3
    end

    @desc "The user's last name"
    field :last_name,
          :string,
          deprecate: "Please use the single 'name' field" do
      resolve &User.get_last_name/3
    end

    @desc "The user's name"
    field :name, :string

    @desc "A freeform tagline for the user"
    field :tagline, :string, do: scope(:public)

    @desc "A list of roles assigned to the user"
    field :roles, non_null(list_of(non_null(:string))), do: scope(:public)

    @desc "The user's hidden state"
    field :hidden, :hidden,
    deprecate: "hidden is no longer supported on the server" do
      resolve fn _, _ -> {:ok, %{}} end
    end

    @desc """
    Timestamp for the last time the user object's data was changed. Applies
    only to the following fields:
    * handle
    * media
    * name (first and last)
    * tagline
    * roles
    """
    field :updated_at, :datetime

    @desc "Bots related to the user specified by either relationship or ID"
    connection field :bots, node_type: :bots do
      connection_complexity()
      arg :relationship, :user_bot_relationship
      arg :id, :uuid
      resolve &Bot.get_bots/3
    end

    @desc """
    The user's contacts optionally filtered by relationship
    """
    connection field :contacts, node_type: :contacts do
      connection_complexity()
      arg :relationship, :user_contact_relationship
      resolve &User.get_contacts/3
    end

    @desc "The user's current presence status"
    field :presence_status,
          :presence_status,
          deprecate: "Please use the single 'presence' field" do
      resolve &User.get_presence_status/3
    end

    @desc "The user's presence data"
    field :presence, :presence, resolve: &User.get_presence/3

    resolve_type fn
      %{id: id}, %{context: %{current_user: %{id: id}}} -> :current_user
      %Wocky.User{} = _, _ -> :other_user
      _, _ -> nil
    end
  end

  @desc "A user other than the currently authenticated user"
  object :other_user do
    interface :user
    import_fields :user
  end

  @desc "The currently authenticated user"
  object :current_user do
    scope :private
    interface :user
    import_fields :user

    @desc "The user's ID for the external auth system (eg Firebase or Digits)"
    field :external_id, :string

    @desc "The user's phone number in E.164 international notation"
    field :phone_number, :string

    @desc "The user's email address"
    field :email, :string

    @desc "Whether the user has ever created a bot"
    field :bot_created, :boolean

    @desc """
    Blob for storage of arbitrary client-defined data. Used on the client-side
    only
    """
    field :client_data, :string

    @desc "The active bots to which a user is subscribed, in last visited order"
    connection field :active_bots, node_type: :bots do
      connection_complexity()
      resolve &Bot.get_active_bots/3
    end

    @desc "The user's location history for a given device"
    connection field :locations, node_type: :locations do
      connection_complexity()
      arg :device, non_null(:string)
      resolve &User.get_locations/3
    end

    @desc "The user's location event history"
    connection field :location_events, node_type: :location_events do
      connection_complexity()
      arg :device, non_null(:string)
      resolve &User.get_location_events/3
    end

    @desc "The user's live location sharing sessions"
    connection field :location_shares, node_type: :user_location_live_shares do
      connection_complexity()
      resolve &User.get_location_shares/3
    end

    @desc "The live location sharing sessions with the user"
    connection field :location_sharers, node_type: :user_location_live_shares do
      connection_complexity()
      resolve &User.get_location_sharers/3
    end

    @desc "The user's archive of messages sorted from oldest to newest"
    connection field :messages, node_type: :messages do
      connection_complexity()

      @desc "Optional other user to filter messages on"
      arg :other_user, :uuid
      resolve &Message.get_messages/3
    end

    @desc """
    The user's conversations - i.e. the last message exchanged with each contact
    """
    connection field :conversations, node_type: :conversations do
      connection_complexity()
      resolve &Message.get_conversations/3
    end

    @desc """
    The user's contacts (ie the XMPP roster) optionally filtered by relationship
    DEPRECATED. Use "friends" connection instead.
    """
    connection field :contacts,
                 node_type: :contacts,
                 deprecate: """
                 Use the 'friends', 'sent_invitations',
                 and 'received_invitations' fields instead
                 """ do
      connection_complexity()
      arg :relationship, :user_contact_relationship
      resolve &User.get_contacts/3
    end

    connection field :friends, node_type: :friends do
      connection_complexity()
      resolve &User.get_friends/3
    end

    connection field :sent_invitations, node_type: :invitations do
      connection_complexity()
      resolve &User.get_sent_invitations/3
    end

    connection field :received_invitations, node_type: :invitations do
      connection_complexity()
      resolve &User.get_received_invitations/3
    end

    @desc "Other users that this user has blocked"
    connection field :blocks, node_type: :blocks do
      connection_complexity()
      resolve &Block.get_blocks/3
    end
  end

  enum :user_bot_relationship do
    @desc "A bot is visible to the user"
    value :visible

    @desc "A bot is owned by the user"
    value :owned

    @desc "A user has been invited to a bot"
    value :invited

    @desc "The user has subscribed to the bot (including owned bots)"
    value :subscribed

    @desc "The user has subscribed to the bot and does not own it"
    value :subscribed_not_owned

    @desc "The user is a guest of the bot (will fire entry/exit events)"
    value :guest, deprecate: "All subscribers are now guests"

    @desc "The user is a visitor to the bot (is currently within the bot)"
    value :visitor
  end

  enum :user_contact_relationship do
    @desc "The parent user has invited the child user to be a friend"
    value :invited

    @desc "The child user has invited the user to be a friend"
    value :invited_by

    @desc "The two users are friends"
    value :friend

    @desc "The users have no relationship"
    value :none

    @desc "The user is the requesting user"
    value :self

    @desc "DEPRECATED - use INVITED instead"
    value :follower

    @desc "DEPRECATED - use INVITED_BY instead"
    value :following
  end

  @desc "Another user with whom a relationship exists"
  object :contact do
    @desc "The other user"
    field :user, non_null(:user), resolve: &User.get_contact_user/3

    @desc "The current user's relationship with the other user"
    field :relationship, :user_contact_relationship

    @desc "The current user's nickname for the other user"
    field :name, :string

    @desc "The creation time of the contact"
    field :created_at, non_null(:datetime)
  end

  connection :contacts, node_type: :user do
    total_count_field()

    edge do
      @desc "The relationship between the parent and child users"
      field :relationship, :user_contact_relationship,
        do: resolve(&User.get_contact_relationship/3)

      @desc "When the relationship was created"
      field :created_at, non_null(:datetime),
        do: resolve(&User.get_contact_created_at/3)
    end
  end

  connection :friends, node_type: :contact do
    total_count_field()

    edge do
    end
  end

  connection :invitations, node_type: :invitation do
    total_count_field()

    edge do
    end
  end

  @desc "An invitation from one user to another to become a friend"
  object :invitation do
    @desc "The sender"
    field :sender, :user, resolve: dataloader(Wocky, :user)

    @desc "The recipient"
    field :recipient, :user, resolve: dataloader(Wocky, :invitee)

    @desc "When the invitation was created"
    field :created_at, non_null(:datetime)
  end

  @desc "A user location update entry"
  object :location do
    @desc "Latitude in degrees"
    field :lat, non_null(:float)

    @desc "Longitude in degrees"
    field :lon, non_null(:float)

    @desc "Reported accuracy in meters"
    field :accuracy, non_null(:float)

    @desc "Timestamp when the report was captured on the device"
    field :captured_at, :datetime

    @desc "Unique ID of the location report"
    field :id, :uuid

    @desc "Time of location report"
    field :created_at, non_null(:datetime)

    @desc "List of events triggered by this location update"
    connection field :events, node_type: :location_events do
      connection_complexity()
      resolve &User.get_location_events/3
    end
  end

  connection :locations, node_type: :location do
    total_count_field()

    edge do
    end
  end

  @desc "A user location event entry"
  object :location_event do
    @desc "The bot whose boundary was entered or exited"
    field :bot, non_null(:bot), resolve: dataloader(Wocky)

    @desc "The type of the event (enter, exit, etc)"
    field :event, non_null(:location_event_type)

    @desc "Time when the event was created"
    field :created_at, non_null(:datetime)

    @desc "The location update that triggered this event (if any)"
    field :location, :location, resolve: dataloader(Wocky)
  end

  @desc "User location event type"
  enum :location_event_type do
    @desc "User is inside a bot's perimeter"
    value :enter

    @desc "User is outside a bot's perimeter"
    value :exit

    @desc "User has entered a bot's perimeter and debouncing has started"
    value :transition_in

    @desc "User has exited a bot's perimeter and debouncing has started"
    value :transition_out

    @desc "User has not sent location updates in some time and is now inactive"
    value :timeout

    @desc "User has reappeared after timeout while inside a bot's perimeter"
    value :reactivate

    @desc "User has reappeared after timeout while outside a bot's perimeter"
    value :deactivate
  end

  connection :location_events, node_type: :location_event do
    total_count_field()

    edge do
    end
  end

  @desc "The state of the user's hidden mode"
  object :hidden do
    @desc "Whether the user is currently hidden"
    field :enabled, non_null(:boolean),
      deprecate: "hidden is no longer supported by the server",
      resolve: fn _, _ -> {:ok, false} end

    @desc """
    When the current or last hidden state expires/expired. Null if no
    expiry is/was scheduled.
    """
    field :expires, :datetime,
      deprecate: "hidden is no longer supported by the server",
      resolve: fn _, _ -> {:ok, DateTime.from_unix!(0)} end
  end

  @desc "Parameters for modifying a user"
  input_object :user_params do
    field :handle, :string
    field :image_url, :string
    # Deprecated in favour of `name`:
    field :first_name, :string
    # Deprecated in favour of `name`:
    field :last_name, :string
    field :name, :string
    field :email, :string
    field :tagline, :string
    field :client_data, :string

    @desc """
    Setting a user as transient allows that user to be deleted AT WILL by the
    server.  This should be set on users which are created as one-shot tests so
    that the server knows they can be safely cleaned up at a later point.
    """
    field :transient, :boolean
  end

  input_object :user_update_input do
    field :values, non_null(:user_params)
  end

  input_object :user_hide_input do
    @desc "Enable or disable hidden/invisible mode"
    field :enable, non_null(:boolean)

    @desc """
    Timestamp of when to expire hidden mode, if enabled. If not present,
    hidden mode will remain on until explicitly disabled.
    """
    field :expire, :datetime
  end

  input_object :follow_input do
    @desc "The ID of the user to start following"
    field :user_id, non_null(:uuid)
  end

  input_object :friend_invite_input do
    @desc "The ID of the user to invite to be a friend"
    field :user_id, non_null(:uuid)
  end

  input_object :friend_delete_input do
    @desc """
    The ID of the user remove as a friend or whose invitation to decline
    or cancel
    """
    field :user_id, non_null(:uuid)
  end

  input_object :friend_name_input do
    @desc "The ID of the user to whom to assign a name"
    field :user_id, non_null(:uuid)

    @desc "The name to assign to the specified user"
    field :name, non_null(:string)
  end

  payload_object(:user_update_payload, :user)
  payload_object(:user_hide_payload, :boolean)
  payload_object(:follow_payload, :contact)
  payload_object(:friend_invite_payload, :user_contact_relationship)
  payload_object(:friend_delete_payload, :boolean)
  payload_object(:friend_name_payload, :boolean)

  # This definition is an almost straight copy from the payload_object macro.
  # However we need to make the scope public because the object permissions
  # get checked after the user is deleted, and the macro doesn't allow us to do
  # that
  object :user_delete_payload do
    scope :public

    @desc "Indicates if the mutation completed successfully or not. "
    field :successful, non_null(:boolean)

    @desc """
    A list of failed validations. May be blank or null if mutation succeeded.
    """
    field :messages, list_of(:validation_message)

    @desc "The object created/updated/deleted by the mutation"
    field :result, :boolean
  end

  object :user_queries do
    @desc "Retrive the currently authenticated user"
    field :current_user, :current_user do
      resolve &User.get_current_user/3
    end

    @desc "Retrive a user by ID"
    field :user, :user do
      arg :id, non_null(:uuid)
      resolve &User.get_user/3
    end

    @desc "Search for users by first name, last name and handle"
    field :users, list_of(non_null(:user)) do
      @desc "String to match against names and handle"
      arg :search_term, non_null(:string)

      @desc "Maximum number of results to return"
      arg :limit, :integer

      resolve &User.search_users/3
    end
  end

  object :user_mutations do
    @desc "Modify an existing user"
    field :user_update, type: :user_update_payload do
      arg :input, non_null(:user_update_input)
      resolve &User.update_user/3
      middleware WockyAPI.Middleware.RefreshCurrentUser
      changeset_mutation_middleware()
    end

    @desc "Delete the current user"
    field :user_delete, type: :user_delete_payload do
      resolve &User.delete/3
      middleware WockyAPI.Middleware.RefreshCurrentUser
      changeset_mutation_middleware()
    end

    @desc "Hide the current user"
    field :user_hide,
      type: :user_hide_payload,
      deprecate: "hidden mode is no longer supported by the server" do
      arg :input, non_null(:user_hide_input)
      resolve &User.hide/3
    end
  end

  object :contact_mutations do
    @desc "Start following another user"
    field :follow,
      type: :follow_payload,
      deprecate: "Use friendInvite instead" do
      arg :input, non_null(:follow_input)
      resolve &User.invite/3
      changeset_mutation_middleware()
    end

    @desc """
    Invite another user to be your friend or accept an existing invitation from
    them
    """
    field :friend_invite, type: :friend_invite_payload do
      arg :input, non_null(:friend_invite_input)
      resolve &User.invite/3
      changeset_mutation_middleware()
    end

    @desc """
    Remove the friendship between the sender and another user or decline
    or cancel an invitation from or to them
    """
    field :friend_delete, type: :friend_delete_payload do
      arg :input, non_null(:friend_delete_input)
      resolve &User.unfriend/3
      changeset_mutation_middleware()
    end

    @desc "Sets the nickname for a friend"
    field :friend_name, type: :friend_name_payload do
      arg :input, non_null(:friend_name_input)
      resolve &User.name_friend/3
      changeset_mutation_middleware()
    end
  end

  input_object :user_invite_redeem_code_input do
    @desc "The invite code to redeem"
    field :code, non_null(:string)
  end

  payload_object(:user_invite_make_code_payload, :string)
  payload_object(:user_invite_redeem_code_payload, :boolean)

  object :user_invite_code_mutations do
    @desc "Generate a user invite code"
    field :user_invite_make_code, type: :user_invite_make_code_payload do
      resolve &User.make_invite_code/3
    end

    @desc "Redeem a user invite code"
    field :user_invite_redeem_code, type: :user_invite_redeem_code_payload do
      arg :input, non_null(:user_invite_redeem_code_input)
      resolve &User.redeem_invite_code/3
    end
  end

  enum :notification_platform do
    @desc "Apple Push Notification service"
    value :apns

    @desc "Android Firebase Cloud Messaging service"
    value :fcm
  end

  input_object :push_notifications_enable_input do
    @desc "The unique ID for this device"
    field :device, non_null(:string)

    @desc "The notification platform for this device. Defaults to 'apns'."
    field :platform, :notification_platform

    @desc "The platform-specific device token"
    field :token, non_null(:string)

    @desc "Whether to use the dev mode sandbox. Defaults to false."
    field :dev_mode, :boolean
  end

  payload_object(:push_notifications_enable_payload, :boolean)

  input_object :push_notifications_disable_input do
    @desc "The unique ID for this device"
    field :device, non_null(:string)
  end

  payload_object(:push_notifications_disable_payload, :boolean)

  object :push_notifications_mutations do
    @desc "Enable push notifications for this device"
    field :push_notifications_enable, type: :push_notifications_enable_payload do
      arg :input, non_null(:push_notifications_enable_input)
      resolve &User.enable_notifications/2
      changeset_mutation_middleware()
    end

    @desc "Disable push notifications for this device"
    field :push_notifications_disable, type: :push_notifications_disable_payload do
      arg :input, non_null(:push_notifications_disable_input)
      resolve &User.disable_notifications/2
      changeset_mutation_middleware()
    end
  end

  @desc "Parameters for sending a location update"
  input_object :user_location_update_input do
    @desc "The unique ID for the device sending the update"
    field :device, non_null(:string)

    @desc "Latitude in degrees"
    field :lat, non_null(:float)

    @desc "Longitude in degrees"
    field :lon, non_null(:float)

    @desc "Accuracy in metres"
    field :accuracy, non_null(:float)

    @desc "Reported speed in meters"
    field :speed, :float

    @desc "Reported heading in degrees"
    field :heading, :float

    @desc "Reported altitude in meters"
    field :altitude, :float

    @desc "Accuracy of altitude in meters"
    field :altitude_accuracy, :float

    @desc "Timestamp when the report was captured on the device"
    field :captured_at, :datetime

    @desc "Unique ID of the location report"
    field :uuid, :string

    @desc "Whether the device is moving"
    field :is_moving, :boolean

    @desc "Reported total distance in meters"
    field :odometer, :float

    @desc "Reported activity when the report was captured"
    field :activity, :string

    @desc "Percentage confidence in the activity"
    field :activity_confidence, :integer

    @desc "Battery level 0-100%"
    field :battery_level, :float

    @desc "Is the device plugged in?"
    field :battery_charging, :boolean

    @desc "DEPRECATED This field is ignored"
    field :is_fetch, :boolean, deprecate: "This field will be ignored"
  end

  payload_object(:user_location_update_payload, :boolean)

  payload_object(:user_location_get_token_payload, :string)

  @desc "Parameters for starting a live location share"
  input_object :user_location_live_share_input do
    @desc "The user with whom to share location"
    field :shared_with_id, non_null(:string)

    @desc "The expiry for the share"
    field :expires_at, non_null(:datetime)

    @desc "The user's current location"
    field :location, :user_location_update_input
  end

  @desc "Attributes of a user location live sharing session"
  object :user_location_live_share do
    @desc "ID for this sharing session"
    field :id, non_null(:integer)

    @desc "The user sharing their location"
    field :user, non_null(:other_user)

    @desc "The user with whom the location is being shared"
    field :shared_with, non_null(:other_user)

    @desc "When the share was created"
    field :created_at, non_null(:datetime)

    @desc "The expiry for the share"
    field :expires_at, non_null(:datetime)
  end

  connection :user_location_live_shares, node_type: :user_location_live_share do
    total_count_field()

    edge do
    end
  end

  payload_object(:user_location_live_share_payload, :user_location_live_share)

  @desc "Parameters for canceling a live location share"
  input_object :user_location_cancel_share_input do
    @desc "The user whose location sharing to cancel"
    field :shared_with_id, non_null(:string)
  end

  payload_object(:user_location_cancel_share_payload, :boolean)

  object :location_mutations do
    @desc "Update a user's current location"
    field :user_location_update, type: :user_location_update_payload do
      arg :input, non_null(:user_location_update_input)
      resolve &User.update_location/3
      changeset_mutation_middleware()
    end

    @desc "Generate a new token for location updates"
    field :user_location_get_token, type: :user_location_get_token_payload do
      resolve &User.get_location_token/3
    end

    @desc "Share the user's location"
    field :user_location_live_share, type: :user_location_live_share_payload do
      arg :input, non_null(:user_location_live_share_input)
      resolve &User.live_share_location/3
      changeset_mutation_middleware()
    end

    @desc "Cancel a live location share"
    field :user_location_cancel_share, type: :user_location_cancel_share_payload do
      arg :input, non_null(:user_location_cancel_share_input)
      resolve &User.cancel_location_share/3
      changeset_mutation_middleware()
    end

    @desc "Cancel all live location shares"
    field :user_location_cancel_all_shares,
      type: :user_location_cancel_share_payload do
      resolve &User.cancel_all_location_shares/3
      changeset_mutation_middleware()
    end
  end

  @desc "Presence data for a user"
  object :presence do
    @desc "The user's current status"
    field :status, :presence_status

    @desc """
    The time at which this status was generated. Because of the distributed
    nature of the system, it is possible, though unlikely, that presence updates
    may arrive at the client out of order. This field should be used to identify
    and discard stale updates.
    """
    field :updated_at, :datetime
  end

  enum :presence_status do
    @desc "Online"
    value :online

    @desc "Offline"
    value :offline

    # Maybe other items here such as 'DND'
  end

  @desc "Data that is sent when a user's shared location changes"
  object :user_location_update do
    @desc "The user whose location has changed"
    field :user, non_null(:user)

    @desc "The user's new location"
    field :location, non_null(:location)
  end

  object :user_subscriptions do
    @desc """
    Receive an update when a contact's state (friended/unfriended) changes
    """
    field :contacts, non_null(:contact) do
      user_subscription_config(&User.contacts_subscription_topic/1)
    end

    @desc """
    Recieve an update when a friend's presence status changes
    """
    field :presence, non_null(:user) do
      config fn
        _, %{context: %{current_user: user}} ->
          {:ok,
           topic: User.presence_subscription_topic(user.id),
           catchup: fn -> User.presence_catchup(user) end}

        _, _ ->
          {:error, "This operation requires an authenticated user"}
      end
    end

    @desc """
    Receive an update when a friend's data (eg name, handle) changes
    """
    field :friends, non_null(:user) do
      user_subscription_config(&User.friends_subscription_topic/1)
    end

    @desc """
    Receive an update when a friend's shared location changes
    """
    field :shared_locations, non_null(:user_location_update) do
      config fn
        _, %{context: %{current_user: user}} ->
          {:ok,
           topic: User.location_subscription_topic(user.id),
           catchup: fn -> User.location_catchup(user) end}

        _, _ ->
          {:error, "This operation requires an authenticated user"}
      end
    end
  end
end
