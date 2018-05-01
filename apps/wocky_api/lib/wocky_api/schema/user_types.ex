defmodule WockyAPI.Schema.UserTypes do
  @moduledoc """
  Absinthe types for wocky user
  """

  use WockyAPI.Schema.Notation

  import Kronky.Payload

  alias WockyAPI.Resolvers.Bot
  alias WockyAPI.Resolvers.Collection
  alias WockyAPI.Resolvers.Media
  alias WockyAPI.Resolvers.User

  @desc "The main Wocky user object"
  object :user do
    @desc "The user's unique ID"
    field :id, non_null(:uuid)
    @desc "The server on which the user resides"
    field :server, non_null(:string)
    @desc "The user's unique handle"
    field :handle, :string
    field :avatar, :media, do: resolve(&Media.get_media/3)
    field :first_name, :string
    field :last_name, :string
    @desc "A freeform tagline for the user"
    field :tagline, :string
    @desc "A list of roles assigned to the user"
    field :roles, non_null(list_of(non_null(:string)))

    @desc "Bots related to the user specified by either relationship or ID"
    connection field :bots, node_type: :bots do
      arg :relationship, :user_bot_relationship
      arg :id, :uuid
      resolve &Bot.get_bots/3
    end

    @desc """
    The user's contacts (ie the XMPP roster) optionally filtered by relationship
    """
    connection field :contacts, node_type: :contacts do
      arg :relationship, :user_contact_relationship
      resolve &User.get_contacts/3
    end

    @desc "The user's owned bot collections"
    connection field :collections, node_type: :collections do
      resolve &Collection.get_collections/3
    end

    @desc "Bot collections to which the user is subscribed"
    connection field :subscribed_collections, node_type: :collections do
      resolve &Collection.get_subscribed_collections/3
    end
  end

  object :current_user do
    import_fields :user

    @desc "The user's ID for the external auth system (eg Firebase or Digits)"
    field :external_id, :string
    @desc "The user's phone number in E.123 international notation"
    field :phone_number, :string
    field :email, :string

    connection field :active_bots, node_type: :bots do
      resolve &Bot.get_active_bots/3
    end

    @desc "The user's location history for a given device"
    connection field :locations, node_type: :locations do
      arg :device, non_null(:string)
      resolve &User.get_locations/3
    end

    @desc "The user's home stream items"
    connection field :home_stream, node_type: :home_stream do
      resolve &User.get_home_stream/3
    end

    @desc """
    The user's conversations - i.e. the last message exchanged with each contact
    """
    connection field :conversations, node_type: :conversations do
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
    @desc "The user has subscribed to the bot"
    value :subscribed
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
  end

  connection :locations, node_type: :location do
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
    field :user, :user
    @desc "The bot referenced by this item, if any"
    field :reference_bot, :bot
    @desc "The collection referenced by this item, if any"
    field :reference_collection, :bot
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
    @desc "The contents of the message"
    field :message, non_null(:string)

    @desc """
    True if the message was sent from the user,
    false if it was received by them.
    """
    field :outgoing, non_null(:boolean)
    @desc "The owner of the conversation"
    field :user, :user, do: resolve(&User.get_conversation_user/3)
  end

  connection :conversations, node_type: :conversation do
    total_count_field

    edge do
    end
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

  payload_object(:user_update_payload, :user)

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
  end

  payload_object(:user_location_update_payload, :boolean)

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
end
