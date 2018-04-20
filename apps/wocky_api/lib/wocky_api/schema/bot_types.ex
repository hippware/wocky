defmodule WockyAPI.Schema.BotTypes do
  @moduledoc """
  Absinthe types for wocky bot
  """

  use Absinthe.Schema.Notation
  use Absinthe.Relay.Schema.Notation, :modern

  import Kronky.Payload

  alias WockyAPI.Resolvers.Bot
  alias WockyAPI.Resolvers.Collection
  alias WockyAPI.Resolvers.Media
  alias WockyAPI.Resolvers.User
  alias WockyAPI.Resolvers.Utils

  connection :bots, node_type: :bot do
    field :total_count, :integer do
      resolve &Utils.get_count/3
    end
    edge do
      field :relationships, list_of(:user_bot_relationship) do
        resolve &Bot.get_bot_relationships/3
      end
    end
  end

  enum :subscription_type do
    value :subscriber
    value :guest
    value :visitor
  end

  object :bot do
    field :id, non_null(:uuid)
    field :server, non_null(:string)
    field :title, non_null(:string)
    field :lat, non_null(:float), do: resolve &Bot.get_lat/3
    field :lon, non_null(:float), do: resolve &Bot.get_lon/3
    field :radius, non_null(:float)
    field :description, :string
    field :shortname, :string
    field :image, :media, do: resolve &Media.get_media/3
    field :type, :string
    field :address, :string
    field :address_data, :string
    field :public, non_null(:boolean)
    field :geofence, non_null(:boolean)
    field :owner, non_null(:user), do: resolve &User.get_object_owner/3

    connection field :items, node_type: :bot_items do
      resolve &Bot.get_items/3
    end

    connection field :subscribers, node_type: :subscribers do
      arg :type, :subscription_type
      arg :id, :uuid
      resolve &Bot.get_subscribers/3
    end

    connection field :collections, node_type: :collections do
      resolve &Collection.get_collections/3
    end
  end

  object :bot_item do
    field :id, non_null(:string)
    field :stanza, :string
    field :media, :media, do: resolve &Media.get_media/3
    field :image, :boolean
    field :owner, :user, do: resolve &User.get_object_owner/3
  end

  connection :bot_items, node_type: :bot_item do
    field :total_count, non_null(:integer) do
      resolve &Utils.get_count/3
    end
    edge do
    end
  end

  connection :subscribers, node_type: :user do
    field :total_count, non_null(:integer) do
      resolve &Utils.get_count/3
    end
    edge do
      field :relationships, non_null(list_of(:user_bot_relationship)) do
        resolve &Bot.get_bot_relationships/3
      end
    end
  end

  input_object :bot_params do
    field :title, :string
    field :server, :string
    field :lat, :float
    field :lon, :float
    field :radius, :float
    field :description, :string
    field :shortname, :string
    field :image, :string
    field :type, :string
    field :address, :string
    field :address_data, :string
    field :public, :boolean
    field :geofence, :boolean
  end

  input_object :bot_create_input do
    field :values, non_null(:bot_params)
  end

  input_object :bot_update_input do
    field :id, non_null(:uuid)
    field :values, non_null(:bot_params)
  end

  payload_object(:bot_create_payload, :bot)
  payload_object(:bot_update_payload, :bot)

  object :bot_queries do
    field :bot, :bot do
      arg :id, non_null(:uuid)
      resolve &Bot.get_bot/3
    end
  end

  object :bot_mutations do
    field :bot_create, type: :bot_create_payload do
      arg :input, non_null(:bot_create_input)
      resolve &Bot.create_bot/3
      middleware &Utils.fix_changeset/2
      middleware &build_payload/2
    end

    field :bot_update, type: :bot_update_payload do
      arg :input, non_null(:bot_update_input)
      resolve &Bot.update_bot/3
      middleware &Utils.fix_changeset/2
      middleware &build_payload/2
    end

    payload field :bot_subscribe do
      input do
        field :id, non_null(:uuid)
        field :guest, :boolean
      end
      output do
        field :result, :boolean
      end
      resolve &Bot.subscribe/3
    end

    payload field :bot_unsubscribe do
      input do
        field :id, non_null(:uuid)
      end
      output do
        field :result, :boolean
      end
      resolve &Bot.unsubscribe/3
    end
  end

  enum :visitor_action do
    value :arrive
    value :depart
  end

  object :visitor_update do
    field :bot, :bot
    field :visitor, :user
    field :action, :visitor_action
  end

  object :bot_subscriptions do
    field :bot_guest_visitors, non_null(:visitor_update) do
      config fn
        _, %{context: %{current_user: user}} ->
          {:ok, topic: Bot.visitor_subscription_topic(user.id)}
        _, _ ->
          {:error, "This operation requires an authenticated user"}
      end
    end
  end
end
