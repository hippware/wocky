defmodule WockyAPI.Schema.BotTypes do
  @moduledoc """
  Absinthe types for wocky bot
  """

  use Absinthe.Schema.Notation
  use Absinthe.Relay.Schema.Notation, :modern

  import Kronky.Payload

  alias WockyAPI.Resolvers.Bot
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
    field :image, :string
    field :type, :string
    field :address, :string
    field :address_data, :string
    field :public, non_null(:boolean)
    field :geofence, non_null(:boolean)
    field :owner, non_null(:user), do: resolve &Bot.get_owner/3

    connection field :items, node_type: :bot_items do
      resolve &Bot.get_items/3
    end

    connection field :subscribers, node_type: :subscribers do
      arg :type, :subscription_type
      arg :id, :uuid
      resolve &Bot.get_subscribers/3
    end
  end

  object :bot_item do
    field :id, non_null(:string)
    field :stanza, :string
    field :image, :boolean
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

  input_object :create_bot_input do
    field :values, non_null(:bot_params)
  end

  input_object :update_bot_input do
    field :id, non_null(:uuid)
    field :values, non_null(:bot_params)
  end

  payload_object(:create_bot_payload, :bot)
  payload_object(:update_bot_payload, :bot)

  object :bot_queries do
    field :bot, :bot do
      arg :id, non_null(:uuid)
      resolve &Bot.get_bot/3
    end
  end

  object :bot_mutations do
    field :create_bot, type: :create_bot_payload do
      arg :input, non_null(:create_bot_input)
      resolve &Bot.create_bot/3
      middleware &Utils.fix_changeset/2
      middleware &build_payload/2
    end

    field :update_bot, type: :update_bot_payload do
      arg :input, non_null(:update_bot_input)
      resolve &Bot.update_bot/3
      middleware &Utils.fix_changeset/2
      middleware &build_payload/2
    end

    payload field :subscribe_bot do
      input do
        field :id, non_null(:uuid)
        field :guest, :boolean
      end
      output do
        field :result, :boolean
      end
      resolve &Bot.subscribe/3
    end

    payload field :unsubscribe_bot do
      input do
        field :id, non_null(:uuid)
      end
      output do
        field :result, :boolean
      end
      resolve &Bot.unsubscribe/3
    end
  end

  object :bot_subscriptions do
    field :bot_visitors, non_null(:bot) do
      arg :id, non_null(:uuid)

      config fn args, _ -> {:ok, topic: args.id} end
    end
  end
end
