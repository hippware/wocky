defmodule WockyAPI.Schema do
  @moduledoc "GraphQL schema for the Wocky API"

  use Absinthe.Schema
  use Absinthe.Relay.Schema, :modern

  import Kronky.Payload

  import_types Kronky.ValidationMessageTypes

  alias WockyAPI.BotResolver
  alias WockyAPI.UserResolver
  alias WockyAPI.UtilResolver

  object :user do
    field :id, non_null(:id)
    field :server, non_null(:string)
    field :handle, :string
    field :avatar, :string
    field :first_name, :string
    field :last_name, :string
    field :tagline, :string
    field :roles, non_null(list_of(non_null(:string)))
    field :external_id, :string
    field :phone_number, :string
    field :email, :string

    connection field :bots, node_type: :bots do
      arg :relationship, non_null(:user_bot_relationship)
      resolve &UserResolver.get_bots/3
    end

    connection field :contacts, node_type: :contacts do
      arg :relationship, :user_contact_relationship
      resolve &UserResolver.get_contacts/3
    end

    connection field :home_stream, node_type: :home_stream do
      resolve &UserResolver.get_home_stream/3
    end

    connection field :conversations, node_type: :conversations do
      resolve &UserResolver.get_conversations/3
    end
  end

  object :private_user_data do
  end

  enum :user_bot_relationship do
    value :visible
    value :owned
    value :shared
    value :subscribed
    value :guest
    value :visitor
  end

  connection :bots, node_type: :bot do
    field :total_count, :integer do
      resolve &UtilResolver.get_count/3
    end
    edge do
      field :relationships, list_of(:user_bot_relationship) do
        resolve &UserResolver.get_bot_relationships/3
      end
    end
  end

  enum :subscription_type do
    value :subscriber
    value :guest
    value :visitor
  end

  object :bot do
    field :id, non_null(:id)
    field :server, non_null(:string)
    field :title, non_null(:string)
    field :lat, non_null(:float), do: resolve &BotResolver.get_lat/3
    field :lon, non_null(:float), do: resolve &BotResolver.get_lon/3
    field :radius, non_null(:float)
    field :description, :string
    field :shortname, :string
    field :image, :string
    field :type, :string
    field :address, :string
    field :address_data, :string
    field :public, non_null(:boolean)
    field :geofence, non_null(:boolean)

    connection field :items, node_type: :bot_items do
      resolve &BotResolver.get_items/3
    end

    connection field :subscribers, node_type: :subscribers do
      arg :type, :subscription_type
      resolve &BotResolver.get_subscribers/3
    end
  end

  enum :user_contact_relationship do
    value :following
    value :follower
    value :friend
  end

  connection :contacts, node_type: :user do
    field :total_count, :integer do
      resolve &UtilResolver.get_count/3
    end
    edge do
      field :relationship, :user_contact_relationship do
        resolve &UserResolver.get_contact_relationship/3
      end
    end
  end

  object :home_stream_item do
    field :id, non_null(:id)
    # ...
  end

  connection :home_stream, node_type: :home_stream_item do
    field :total_count, :integer do
      resolve &UtilResolver.get_count/3
    end
    edge do
    end
  end

  object :conversation do
    field :id, non_null(:id)
    # ...
  end

  connection :conversations, node_type: :conversation do
    field :total_count, :integer do
      resolve &UtilResolver.get_count/3
    end
    edge do
    end
  end

  object :bot_item do
    field :id, non_null(:string)
    field :stanza, :string
    field :image, :boolean
  end

  connection :bot_items, node_type: :bot_item do
    field :total_count, :integer do
      resolve &UtilResolver.get_count/3
    end
    edge do
    end
  end

  connection :subscribers, node_type: :user do
    field :total_count, :integer do
      resolve &UtilResolver.get_count/3
    end
    edge do
      field :type, :subscription_type do
        resolve &BotResolver.get_subscription_type/3
      end
    end
  end

  input_object :create_bot_params do
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

  payload_object(:bot_payload, :bot)

  object :bot_mutations do
    field :create_bot, type: :bot_payload do
      arg :id, :id
      arg :bot, non_null(:create_bot_params)
      resolve &BotResolver.insert_bot/3
      middleware &UtilResolver.fix_changeset/2
      middleware &build_payload/2
    end
  end

  input_object :update_user_params do
    field :handle, :string
    field :avatar, :string
    field :first_name, :string
    field :last_name, :string
    field :email, :string
    field :tagline, :string
  end

  payload_object(:user_payload, :user)

  object :user_mutations do
    field :update_user, type: :user_payload do
      arg :user, non_null(:update_user_params)
      resolve &UserResolver.update_user/3
      middleware &UtilResolver.fix_changeset/2
      middleware &build_payload/2
    end
  end

  query do
    field :current_user, non_null(:user) do
      resolve &UserResolver.get_current_user/3
    end

    # TODO: Add blocking support to bot resolution
    field :bot, :bot do
      arg :id, non_null(:id)
      resolve &BotResolver.get_bot/3
    end

    # TODO: Implement blocking handling for this
    field :user, :user do
      arg :id, non_null(:string)
      resolve &UserResolver.get_user/3
    end

    # Leave public stuff unavailable until we sort out the details
    #field :public_bot, :bot do
    #  arg :id, non_null(:string)
    #
    #  resolve &BotResolver.get_public_bot/3
    #end
  end

  mutation do
    import_fields :user_mutations
    import_fields :bot_mutations
  end

  subscription do
    field :bot_visitors, non_null(:bot) do
      arg :id, non_null(:id)

      config fn args, a -> {:ok, topic: args.id} end
    end
  end

  # For now all requests must be authenticated
  # In the future this is where we can distinguish public/private data
  def middleware(middleware, %{identifier: field}, %{identifier: :user})
  when field == :external_id
    or field == :phone_number
    or field == :email do
    [WockyAPI.Middleware.AuthSelf| middleware]
  end

  def middleware(middleware, _field, _object) do
    [WockyAPI.Middleware.Auth | middleware]
  end

end
