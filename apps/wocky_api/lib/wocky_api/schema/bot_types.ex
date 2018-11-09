defmodule WockyAPI.Schema.BotTypes do
  @moduledoc """
  Absinthe types for wocky bot
  """

  use WockyAPI.Schema.Notation
  use Absinthe.Ecto, repo: Wocky.Repo

  import Kronky.Payload

  alias WockyAPI.Resolvers.Bot
  alias WockyAPI.Resolvers.Media
  alias WockyAPI.Resolvers.Utils

  connection :bots, node_type: :bot do
    total_count_field()

    edge do
      @desc "The set of relationships between the user and the bot"
      field :relationships, list_of(:user_bot_relationship) do
        resolve &Bot.get_bot_relationships/3
      end
    end
  end

  enum :subscription_type do
    @desc "A user who is subscribed to the bot"
    value :subscriber

    @desc """
    A user who is subscribed to the bot and who is currently visiting it
    """
    value :visitor

    @desc "Deprecated"
    value :guest, deprecate: "All subscribers are now guests"
  end

  @desc "A Wocky bot"
  object :bot do
    @desc "The bot's unique ID"
    field :id, non_null(:uuid)

    @desc "The server on which the bot resides"
    field :server, non_null(:string) do
      resolve &Utils.server_resolver/3
      deprecate "server is deprecated and should be ignored"
    end

    @desc "The bot's title"
    field :title, non_null(:string)

    @desc "The bot's latitude in degrees"
    field :lat, non_null(:float), resolve: &Bot.get_lat/3

    @desc "The bot's longitude in degrees"
    field :lon, non_null(:float), resolve: &Bot.get_lon/3

    @desc "The bot's geofence radius in metres"
    field :radius, non_null(:float)

    @desc "The full description of the bot"
    field :description, :string

    @desc "The bot's unique short name"
    field :shortname, :string

    @desc "The bot's cover image"
    field :image, :media, resolve: &Media.get_media/3

    @desc "The type of the bot (freeform string, client-side use only)"
    field :type, :string

    @desc "The icon for the bot (freeform string, client-side use only)"
    field :icon, :string

    @desc "The bot's street address"
    field :address, :string

    @desc "Extra address data (freeform string, client-side use only)"
    field :address_data, :string

    @desc "Whether the bot is publicly visible"
    field :public, non_null(:boolean), do: deprecate("All bots are now private")

    @desc "Whether the bot has geofence (visitor reporting) enabled"
    field :geofence, non_null(:boolean),
      do: deprecate("All bots are now geofence-enabled")

    @desc "The bot's owner"
    field :owner, non_null(:user), resolve: assoc(:user)

    @desc "Initial creation time of the bot"
    field :created_at, non_null(:datetime)

    @desc "Last time the bot was updated"
    field :updated_at, non_null(:datetime)

    @desc "Posts made to the bot"
    connection field :items, node_type: :bot_items do
      connection_complexity()
      resolve &Bot.get_items/3
    end

    @desc "Subscribers to the bot, filtered by either subscription type or ID"
    connection field :subscribers, node_type: :subscribers do
      connection_complexity()
      arg :type, :subscription_type
      arg :id, :uuid
      resolve &Bot.get_subscribers/3
    end
  end

  @desc "A post (comment, etc) to a bot"
  object :bot_item do
    @desc "The unique ID of this post"
    field :id, non_null(:string)

    @desc "The post's content"
    field :stanza, :string

    @desc "Media contained in the post"
    field :media, :media, do: resolve(&Media.get_media/3)

    @desc "True if the post is an image post"
    field :image, :boolean

    @desc "The post's owner"
    field :owner, :user, resolve: assoc(:user)
  end

  @desc "An invitation to subscribe to a bot"
  object :bot_invitation do
    @desc "The unique ID of the invitation"
    field :id, non_null(:aint)

    @desc "The user who sent the invitation"
    field :user, non_null(:user), resolve: assoc(:user)

    @desc "The recipient of the invitation"
    field :invitee, non_null(:user), resolve: assoc(:invitee)

    @desc "The bot to which the recipient has been invited"
    field :bot, non_null(:bot), resolve: assoc(:bot)

    @desc """
    Whether the invitation has been accepted (true), declined (false), or
    not yet responded to (null)
    """
    field :accepted, :boolean
  end

  object :local_bots do
    @desc "The bots found in the requested area"
    field :bots, non_null(list_of(:bot))

    @desc """
    If true, the area requested was too large to search and no bots will be
    returned
    """
    field :area_too_large, :boolean
  end

  connection :bot_items, node_type: :bot_item do
    total_count_field()

    edge do
    end
  end

  connection :subscribers, node_type: :user do
    total_count_field()

    edge do
      @desc "The set of relationships this subscriber has to the bot"
      field :relationships, non_null(list_of(:user_bot_relationship)) do
        resolve &Bot.get_bot_relationships/3
      end
    end
  end

  @desc "Parameters for creating and updating a bot"
  input_object :bot_params do
    field :title, :string

    field :server, :string do
      deprecate "server field is deprecated and will be ignored"
    end

    field :lat, :float
    field :lon, :float
    field :radius, :float
    field :description, :string
    field :shortname, :string
    field :image, :string
    field :type, :string
    field :icon, :string
    field :address, :string
    field :address_data, :string
    field :public, :boolean, do: deprecate("All bots are now private")

    field :geofence, :boolean,
      do: deprecate("All bots are now geofence-enabled")
  end

  input_object :bot_create_input do
    field :values, non_null(:bot_params)

    @desc "Optional location to immediately apply to user against bot"
    field :user_location, :user_location_update_input
  end

  input_object :bot_update_input do
    @desc "ID of bot to update"
    field :id, non_null(:uuid)

    field :values, non_null(:bot_params)

    @desc "Optional location to immediately apply to user against bot"
    field :user_location, :user_location_update_input
  end

  input_object :bot_item_params do
    @desc """
    ID for the item. If this is not supplied, a new one will be generated.
    NOTE: For backwards compatability, supplying a non-existant ID will
    create a new item with an unrelated ID different from the one provided.
    """
    field :id, :string

    @desc "Content of them item"
    field :stanza, :string
  end

  input_object :bot_delete_input do
    @desc "ID of bot to delete"
    field :id, non_null(:uuid)
  end

  input_object :bot_subscribe_input do
    @desc "ID of bot to which to subscribe"
    field :id, non_null(:uuid)

    @desc "Optional location to immediately apply to user against bot"
    field :user_location, :user_location_update_input

    @desc "Whether to enable guest functionality for the user (default: false)"
    field :guest, :boolean
  end

  input_object :bot_unsubscribe_input do
    @desc "ID of the bot from which to unsubscribe"
    field :id, non_null(:uuid)
  end

  input_object :bot_item_publish_input do
    @desc "ID of the bot containing the item"
    field :bot_id, non_null(:uuid)

    field :values, non_null(:bot_item_params)
  end

  input_object :bot_item_delete_input do
    @desc "ID of the bot containing the item"
    field :bot_id, non_null(:uuid) do
      deprecate "The bot ID is no longer required for deleting items"
    end

    @desc "ID of the item to delete"
    field :id, non_null(:uuid)
  end

  input_object :bot_invite_input do
    @desc "ID of the bot to which the user is invited"
    field :bot_id, non_null(:uuid)

    @desc "Users to invite"
    field :user_ids, non_null(list_of(non_null(:uuid)))
  end

  input_object :bot_invitation_respond_input do
    @desc "ID of the invitation being replied to"
    field :invitation_id, non_null(:aint)

    @desc "Whether the invitation is accepted (true) or declined (false)"
    field :accept, non_null(:boolean)

    @desc "Optional location to immediately apply to user against bot"
    field :user_location, :user_location_update_input
  end

  payload_object(:bot_create_payload, :bot)
  payload_object(:bot_update_payload, :bot)
  payload_object(:bot_delete_payload, :boolean)
  payload_object(:bot_subscribe_payload, :boolean)
  payload_object(:bot_unsubscribe_payload, :boolean)
  payload_object(:bot_item_publish_payload, :bot_item)
  payload_object(:bot_item_delete_payload, :boolean)
  payload_object(:bot_invite_payload, :bot_invitation)
  payload_object(:bot_invitation_respond_payload, :boolean)

  object :bot_queries do
    @desc "Retrieve a single bot by ID"
    field :bot, :bot do
      arg :id, non_null(:uuid)
      resolve &Bot.get_bot/3
    end

    @desc """
    Retrieve owned and subscribed bots in a given region. The query will return
    an empty list of bots if the search radius (the diagonal of the rectangle)
    exceeds #{Bot.max_local_bots_search_radius()} meters.
    """
    field :local_bots, non_null(:local_bots) do
      @desc "Top left of the rectangle in which to search"
      arg :point_a, non_null(:point)

      @desc "Bottom right point of the rectangle in which to search"
      arg :point_b, non_null(:point)

      @desc """
      Maximum bots to return (default is #{Bot.default_local_bots()},
      maximum is #{Bot.max_local_bots()})
      """
      arg :limit, :integer

      resolve &Bot.get_local_bots/3
    end

    @desc "Retrieve a list of discoverable bots created after a given time"
    field :discover_bots, list_of(:string) do
      @desc "Optional time after which bots must have been created"
      arg :since, :datetime

      deprecate "This operation is no longer supported"

      resolve fn _, _, _ -> {:ok, []} end
    end
  end

  object :bot_mutations do
    @desc "Create a new bot"
    field :bot_create, type: :bot_create_payload do
      arg :input, :bot_create_input
      resolve &Bot.create_bot/3
      changeset_mutation_middleware()
    end

    @desc "Update an existing bot"
    field :bot_update, type: :bot_update_payload do
      arg :input, non_null(:bot_update_input)
      resolve &Bot.update_bot/3
      changeset_mutation_middleware()
    end

    @desc "Delete a bot"
    field :bot_delete, type: :bot_delete_payload do
      arg :input, non_null(:bot_delete_input)
      resolve &Bot.delete/3
      changeset_mutation_middleware()
    end

    @desc "Subscribe the current user to a bot"
    field :bot_subscribe, type: :bot_subscribe_payload do
      arg :input, non_null(:bot_subscribe_input)
      resolve &Bot.subscribe/3
      changeset_mutation_middleware()
    end

    @desc "Unsubscribe the current user from a bot"
    field :bot_unsubscribe, type: :bot_unsubscribe_payload do
      arg :input, non_null(:bot_unsubscribe_input)
      resolve &Bot.unsubscribe/3
      changeset_mutation_middleware()
    end

    @desc "Publish an item to a bot"
    field :bot_item_publish, type: :bot_item_publish_payload do
      arg :input, non_null(:bot_item_publish_input)
      resolve &Bot.publish_item/3
      changeset_mutation_middleware()
    end

    @desc "Delete an item from a bot"
    field :bot_item_delete, type: :bot_item_delete_payload do
      arg :input, non_null(:bot_item_delete_input)
      resolve &Bot.delete_item/3
      changeset_mutation_middleware()
    end

    @desc "Invite users to a bot"
    field :bot_invite, type: list_of(:bot_invite_payload) do
      arg :input, non_null(:bot_invite_input)
      resolve &Bot.invite/3
      changeset_list_mutation_middleware()
    end

    @desc "Respond to an invititation"
    field :bot_invitation_respond, type: :bot_invitation_respond_payload do
      arg :input, non_null(:bot_invitation_respond_input)
      resolve &Bot.invitation_respond/3
      changeset_mutation_middleware()
    end
  end

  enum :visitor_action do
    @desc "A visitor newly arriving at a bot"
    value :arrive

    @desc "A visitor newly departing from a bot"
    value :depart
  end

  @desc "An update on the state of a visitor to a bot"
  object :visitor_update do
    @desc "The bot with the visitor"
    field :bot, :bot

    @desc "The user visiting"
    field :visitor, :user

    @desc "Whether the user has arrived or departed"
    field :action, :visitor_action
  end

  object :bot_subscriptions do
    @desc """
    Receive updates on all visitors to all bots of which the current user is
    a guest
    """
    field :bot_guest_visitors, non_null(:visitor_update) do
      user_subscription_config(&Bot.visitor_subscription_topic/1)
    end
  end
end
