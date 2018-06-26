defmodule WockyAPI.Schema.UserTypes do
  @moduledoc """
  Absinthe types for wocky user
  """

  use WockyAPI.Schema.Notation
  use Absinthe.Ecto, repo: Wocky.Repo

  import Kronky.Payload

  alias WockyAPI.Resolvers.Bot
  alias WockyAPI.Resolvers.Collection
  alias WockyAPI.Resolvers.Media
  alias WockyAPI.Resolvers.User
  alias WockyAPI.Resolvers.Utils

  @desc "The main Wocky user interface"
  interface :user do
    @desc "The user's unique ID"
    field :id, non_null(:uuid), do: scope(:public)

    @desc "The server on which the user resides"
    field :server, non_null(:string) do
      scope(:public)
      resolve &Utils.server_resolver/3
    end

    @desc "The user's unique handle"
    field :handle, :string, do: scope(:public)

    @desc "The user's avatar"
    field :avatar, :media do
      scope :public
      resolve &Media.get_media/3
    end

    @desc "The user's first name"
    field :first_name, :string

    @desc "The user's last name"
    field :last_name, :string

    @desc "A freeform tagline for the user"
    field :tagline, :string, do: scope(:public)

    @desc "A list of roles assigned to the user"
    field :roles, non_null(list_of(non_null(:string))), do: scope(:public)

    @desc "The user's hidden state"
    field :hidden, :hidden, do: resolve(&User.get_hidden/3)

    @desc "Bots related to the user specified by either relationship or ID"
    connection field :bots, node_type: :bots do
      scope :public
      connection_complexity
      arg :relationship, :user_bot_relationship
      arg :id, :uuid
      resolve &Bot.get_bots/3
    end

    @desc """
    The user's contacts (ie the XMPP roster) optionally filtered by relationship
    """
    connection field :contacts, node_type: :contacts do
      connection_complexity
      arg :relationship, :user_contact_relationship
      resolve &User.get_contacts/3
    end

    @desc "The user's owned bot collections"
    connection field :collections, node_type: :collections do
      connection_complexity
      resolve &Collection.get_collections/3
    end

    @desc "Bot collections to which the user is subscribed"
    connection field :subscribed_collections, node_type: :collections do
      connection_complexity
      resolve &Collection.get_subscribed_collections/3
    end

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

    @desc "The user's phone number in E.123 international notation"
    field :phone_number, :string

    @desc "The user's email address"
    field :email, :string

    @desc "Check whether a user has made use of any geofence features"
    field :has_used_geofence, :boolean, do: resolve(&User.has_used_geofence/3)

    @desc "The active bots that a user is a guest of, in last visited order"
    connection field :active_bots, node_type: :bots do
      connection_complexity
      resolve &Bot.get_active_bots/3
    end

    @desc "The user's location history for a given device"
    connection field :locations, node_type: :locations do
      connection_complexity
      arg :device, non_null(:string)
      resolve &User.get_locations/3
    end

    @desc "The user's location event history"
    connection field :location_events, node_type: :location_events do
      connection_complexity
      arg :device, non_null(:string)
      resolve &User.get_location_events/3
    end

    @desc "The user's home stream items"
    connection field :home_stream, node_type: :home_stream do
      connection_complexity
      resolve &User.get_home_stream/3
    end

    @desc """
    The user's conversations - i.e. the last message exchanged with each contact
    """
    connection field :conversations, node_type: :conversations do
      connection_complexity
      resolve &User.get_conversations/3
    end
  end

  enum :user_bot_relationship do
    @desc "A bot is visible to the user"
    value :visible

    @desc "A bot is owned by the user"
    value :owned

    @desc "A bot has been explicitly shared to the user"
    value :shared

    @desc "The user has subscribed to the bot (including owned bots)"
    value :subscribed

    @desc "The user has subscribed to the bot and does not own it"
    value :subscribed_not_owned

    @desc "The user is a guest of the bot (will fire entry/exit events)"
    value :guest

    @desc "The user is a visitor to the bot (is currently within the bot)"
    value :visitor
  end

  enum :user_contact_relationship do
    @desc "The parent user is following the child user"
    value :following

    @desc "The child user is following the parent user"
    value :follower

    @desc "The two users are following eachother"
    value :friend
  end

  connection :contacts, node_type: :user do
    total_count_field

    edge do
      @desc "The relationship between the parent and child users"
      field :relationship, :user_contact_relationship do
        resolve &User.get_contact_relationship/3
      end
    end
  end

  @desc "A user location update entry"
  object :location do
    @desc "Latitude in degrees"
    field :lat, non_null(:float)

    @desc "Longitude in degrees"
    field :lon, non_null(:float)

    @desc "Reported accuracy in metres"
    field :accuracy, non_null(:float)

    @desc "Time of location report"
    field :created_at, non_null(:datetime)

    @desc "True if the update is the result of a background fetch"
    field :is_fetch, non_null(:boolean)

    @desc "List of events triggered by this location update"
    connection field :events, node_type: :location_events do
      connection_complexity
      resolve &User.get_location_events/3
    end
  end

  connection :locations, node_type: :location do
    total_count_field

    edge do
    end
  end

  @desc "A user location event entry"
  object :location_event do
    @desc "The bot whose boundary was entered or exited"
    field :bot, non_null(:bot), resolve: assoc(:bot)

    @desc "The type of the event (enter, exit, etc)"
    field :event, non_null(:location_event_type)

    @desc "Time when the event was created"
    field :created_at, non_null(:datetime)

    @desc "The location update that triggered this event (if any)"
    field :location, :location, resolve: assoc(:location)
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
    total_count_field

    edge do
    end
  end

  @desc "An item within a user's home stream"
  object :home_stream_item do
    @desc "The stream-unique key of the item"
    field :key, non_null(:string)

    @desc "The JID of the object from which the item originated"
    field :from_jid, non_null(:string)

    @desc "The item's content"
    field :stanza, non_null(:string)

    @desc "The item's owner"
    field :user, :user, resolve: assoc(:user)

    @desc "The user referenced by this item, if any"
    field :reference_user, :bot, resolve: assoc(:reference_user)

    @desc "The bot referenced by this item, if any"
    field :reference_bot, :bot, resolve: assoc(:reference_bot)

    @desc "The collection referenced by this item, if any"
    field :reference_collection, :collection,
      resolve: assoc(:reference_collection)

    @desc "The time at which the item was last updated"
    field :updated_at, :datetime
  end

  connection :home_stream, node_type: :home_stream_item do
    total_count_field

    edge do
    end
  end

  @desc "The most recent message sent between two users"
  object :conversation do
    @desc "The JID of the remote entity"
    field :other_jid, non_null(:string)

    @desc "The other participant in the conversation"
    field :other_user, :user, do: resolve(&User.get_conversation_user/3)

    @desc "The contents of the message"
    field :message, non_null(:string)

    @desc """
    True if the message was sent from the user,
    false if it was received by them.
    """
    field :outgoing, non_null(:boolean)

    @desc "The owner of the conversation"
    field :owner, :user, resolve: assoc(:user)
  end

  connection :conversations, node_type: :conversation do
    total_count_field

    edge do
    end
  end

  @desc "The state of the user's hidden mode"
  object :hidden do
    @desc "Whether the user is currently hidden"
    field :enabled, non_null(:boolean)

    @desc """
    When the current or last hidden state expires/expired. Null if no
    expiry is/was scheduled.
    """
    field :expires, :datetime
  end

  @desc "Parameters for modifying a user"
  input_object :user_params do
    field :handle, :string
    field :avatar, :string
    field :first_name, :string
    field :last_name, :string
    field :email, :string
    field :tagline, :string
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

  payload_object(:user_update_payload, :user)
  payload_object(:user_hide_payload, :boolean)

  @desc "Parameters for sending a location update"
  input_object :user_location_update_input do
    field :resource, :string do
      deprecate "resource is deprecated in favor of device"
    end

    @desc "The device (resource) sending the update"
    field :device, :string

    @desc "Latitude in degrees"
    field :lat, non_null(:float)

    @desc "Longitude in degrees"
    field :lon, non_null(:float)

    @desc "Accuracy in metres"
    field :accuracy, non_null(:float)

    @desc "True if the update is the result of a background fetch"
    field :is_fetch, :boolean
  end

  payload_object(:user_location_update_payload, :boolean)

  object :user_queries do
    @desc "Retrive the currently authenticated user"
    field :current_user, :current_user do
      resolve &User.get_current_user/3
    end

    @desc "Retrive a user by ID"
    field :user, :user do
      scope :public
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
      changeset_mutation_middleware
    end

    field :user_hide, type: :user_hide_payload do
      arg :input, non_null(:user_hide_input)
      resolve &User.hide/3
      middleware WockyAPI.Middleware.RefreshCurrentUser
      changeset_mutation_middleware
    end
  end

  object :location_mutations do
    @desc "Update a user's current location"
    field :user_location_update, type: :user_location_update_payload do
      arg :input, non_null(:user_location_update_input)
      resolve &User.update_location/3
      changeset_mutation_middleware
    end
  end

  enum :home_stream_item_action do
    @desc "A newly inserted item"
    value :insert

    @desc "A update to an existing item"
    value :update

    @desc "Deletion of an existing item"
    value :delete
  end

  @desc "An update to a user's home stream"
  object :home_stream_update do
    @desc "The home stream item that has been updated"
    field :item, :home_stream_item

    @desc "The type of change that has occurred"
    field :action, :home_stream_item_action
  end

  object :user_subscriptions do
    @desc """
    Receive updates when home stream items are inserted, removed, or updated
    """
    field :home_stream, non_null(:home_stream_update) do
      config fn
        _, %{context: %{current_user: user}} ->
          {:ok, topic: User.home_stream_subscription_topic(user.id)}

        _, _ ->
          {:error, "This operation requires an authenticated user"}
      end
    end
  end
end
