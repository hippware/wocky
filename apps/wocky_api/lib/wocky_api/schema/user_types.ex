defmodule WockyAPI.Schema.UserTypes do
  @moduledoc """
  Absinthe types for wocky user
  """

  use WockyAPI.Schema.Notation

  alias WockyAPI.Resolvers.Block
  alias WockyAPI.Resolvers.Bot
  alias WockyAPI.Resolvers.Contact
  alias WockyAPI.Resolvers.Media
  alias WockyAPI.Resolvers.Message
  alias WockyAPI.Resolvers.Presence
  alias WockyAPI.Resolvers.User

  # -------------------------------------------------------------------
  # Objects

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

    @desc "The user is a visitor to the bot (is currently within the bot)"
    value :visitor
  end

  @desc "The main Wocky user interface"
  interface :user do
    @desc "The user's unique ID"
    field :id, non_null(:uuid)

    @desc "The user's unique handle"
    field :handle, :string

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
    field :tagline, :string

    @desc "A list of roles assigned to the user"
    field :roles, non_null(list_of(non_null(:string)))

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

    @desc "The user's current presence status"
    field :presence_status,
          :presence_status,
          deprecate: "Please use the single 'presence' field" do
      resolve &Presence.get_presence_status/3
    end

    @desc "The user's presence data"
    field :presence, :presence, resolve: &Presence.get_presence/3

    resolve_type fn
      %{id: id}, %{context: %{current_user: %{id: id}}} -> :current_user
      %Wocky.Account.User{} = _, _ -> :other_user
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
      resolve &Bot.get_active_bots/2
    end

    @desc "The user's live location sharing sessions"
    connection field :location_shares, node_type: :user_location_live_shares do
      connection_complexity()
      resolve &Contact.get_location_shares/3
    end

    @desc "The live location sharing sessions with the user"
    connection field :location_sharers, node_type: :user_location_live_shares do
      connection_complexity()
      resolve &Contact.get_location_sharers/3
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

    connection field :friends, node_type: :friends do
      connection_complexity()
      resolve &Contact.get_friends/3
    end

    connection field :sent_invitations, node_type: :invitations do
      connection_complexity()
      resolve &Contact.get_sent_invitations/3
    end

    connection field :received_invitations, node_type: :invitations do
      connection_complexity()
      resolve &Contact.get_received_invitations/3
    end

    @desc "Other users that this user has blocked"
    connection field :blocks, node_type: :blocks do
      connection_complexity()
      resolve &Block.get_blocks/3
    end
  end

  # -------------------------------------------------------------------
  # Queries

  @desc "Single result for userBulkLookup"
  object :user_bulk_lookup_result do
    @desc "The original input phone number for which this is the result set"
    field :phone_number, non_null(:string)

    @desc "The E.164 normalised form of the phone number"
    field :e164_phone_number, :string

    @desc "The user, if any, currently associated with the phone number"
    field :user, :user

    @desc "The relationship of the requestor to the returned user"
    field :relationship, :user_contact_relationship
  end

  object :user_queries do
    @desc "Retrive the currently authenticated user"
    field :current_user, :current_user do
      resolve &User.get_current_user/2
    end

    @desc "Retrive a user by ID"
    field :user, :user do
      arg :id, non_null(:uuid)
      resolve &User.get_user/2
    end

    @desc "Search for users by first name, last name and handle"
    field :users, list_of(non_null(:user)) do
      @desc "String to match against names and handle"
      arg :search_term, non_null(:string)

      @desc "Maximum number of results to return"
      arg :limit, :integer

      resolve &User.get_users/2
    end

    @desc """
    Lookup users by phone number. Numbers not already in E.164 format will be
    attempted to be parsed and normalised based on the user's country (which
    in turn is inferred from their phone number as received from Firebase).

    Bypass numbers will be treated as being in the US.
    """
    field :user_bulk_lookup, type: list_of(:user_bulk_lookup_result) do
      @desc "The list of phone numbers to lookup"
      arg :phone_numbers, non_null(list_of(non_null(:string)))
      resolve &User.get_user_bulk_lookup/2
    end
  end

  # -------------------------------------------------------------------
  # User mutations

  @desc "Parameters for modifying a user"
  input_object :user_params do
    field :handle, :string
    field :image_url, :string
    field :first_name, :string, deprecate: "Please use the single 'name' field"
    field :last_name, :string, deprecate: "Please use the single 'name' field"
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

  payload_object(:user_update_payload, :current_user)

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

  object :user_mutations do
    @desc "Modify an existing user"
    field :user_update, type: :user_update_payload do
      arg :input, non_null(:user_update_input)
      resolve &User.user_update/2
      middleware WockyAPI.Middleware.RefreshCurrentUser
      changeset_mutation_middleware()
    end

    @desc "Delete the current user"
    field :user_delete, type: :user_delete_payload do
      resolve &User.user_delete/2
      middleware WockyAPI.Middleware.RefreshCurrentUser
      changeset_mutation_middleware()
    end
  end

  # -------------------------------------------------------------------
  # Push notification mutations

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
      resolve &User.push_notifications_enable/2
      changeset_mutation_middleware()
    end

    @desc "Disable push notifications for this device"
    field :push_notifications_disable, type: :push_notifications_disable_payload do
      arg :input, non_null(:push_notifications_disable_input)
      resolve &User.push_notifications_disable/2
      changeset_mutation_middleware()
    end
  end

  # -------------------------------------------------------------------
  # User location mutations

  payload_object(:user_location_get_token_payload, :string)

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
  end

  @desc "Response object for user location uploads"
  object :user_location_upload_response do
    @desc "Are any location sharees online"
    field :watched, non_null(:boolean)

    @desc "The number of location sharees that are online"
    field :watchers, non_null(:integer)

    @desc "The time at which watchers went above 0 or below 1"
    field :changed_at, :datetime
  end

  payload_object(:user_location_update_payload, :user_location_upload_response)

  @desc "Parameters for triggering a location update request"
  input_object :user_location_request_trigger_input do
    @desc "The user to trigger the location update request"
    field :user_id, non_null(:uuid)
  end

  payload_object(:user_location_request_trigger_payload, :boolean)

  object :location_mutations do
    @desc "Generate a new token for location updates"
    field :user_location_get_token, type: :user_location_get_token_payload do
      resolve &User.user_location_get_token/2
    end

    @desc "Update a user's current location"
    field :user_location_update, type: :user_location_update_payload do
      arg :input, non_null(:user_location_update_input)
      resolve &User.user_location_update/2
      changeset_mutation_middleware()
    end

    @desc "Trigger a silent push notification request the user's location"
    field :user_location_request_trigger,
      type: :user_location_request_trigger_payload do
      arg :input, non_null(:user_location_request_trigger_input)
      resolve &User.user_location_request_trigger/2
      changeset_mutation_middleware()
    end
  end
end
