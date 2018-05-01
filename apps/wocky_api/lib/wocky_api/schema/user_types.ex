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

  object :user do
    field :id, non_null(:uuid)
    field :server, non_null(:string)
    field :handle, :string
    field :avatar, :media, do: resolve(&Media.get_media/3)
    field :first_name, :string
    field :last_name, :string
    field :tagline, :string
    field :roles, non_null(list_of(non_null(:string)))
    field :external_id, :string
    field :phone_number, :string
    field :email, :string

    connection field :bots, node_type: :bots do
      arg :relationship, :user_bot_relationship
      arg :id, :uuid
      resolve &Bot.get_bots/3
    end

    connection field :locations, node_type: :locations do
      arg :device, non_null(:string)
      resolve &User.get_locations/3
    end

    connection field :contacts, node_type: :contacts do
      arg :relationship, :user_contact_relationship
      resolve &User.get_contacts/3
    end

    connection field :home_stream, node_type: :home_stream do
      resolve &User.get_home_stream/3
    end

    connection field :conversations, node_type: :conversations do
      resolve &User.get_conversations/3
    end

    connection field :collections, node_type: :collections do
      resolve &Collection.get_collections/3
    end

    connection field :subscribed_collections, node_type: :collections do
      resolve &Collection.get_subscribed_collections/3
    end
  end

  enum :user_bot_relationship do
    value :visible
    value :owned
    value :shared
    value :subscribed
    value :guest
    value :visitor
  end

  enum :user_contact_relationship do
    value :following
    value :follower
    value :friend
  end

  connection :contacts, node_type: :user do
    total_count_field

    edge do
      field :relationship, :user_contact_relationship do
        resolve &User.get_contact_relationship/3
      end
    end
  end

  object :location do
    field :lat, non_null(:float)
    field :lon, non_null(:float)
    field :accuracy, non_null(:float)
    field :created_at, non_null(:datetime)
  end

  connection :locations, node_type: :location do
    total_count_field

    edge do
    end
  end

  object :home_stream_item do
    field :key, non_null(:string)
    field :from_jid, non_null(:string)
    field :stanza, non_null(:string)
    field :user, :user
    field :reference_bot, :bot
    field :updated_at, :datetime
  end

  connection :home_stream, node_type: :home_stream_item do
    total_count_field

    edge do
    end
  end

  object :conversation do
    field :other_jid, non_null(:string)
    field :messages, non_null(:string)
    field :outgoing, non_null(:boolean)
    field :user, :user, do: resolve(&User.get_conversation_user/3)
  end

  connection :conversations, node_type: :conversation do
    total_count_field

    edge do
    end
  end

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

  input_object :user_location_update_input do
    field :resource, :string do
      deprecate "resource is deprecated in favor of device"
    end

    field :device, :string
    field :lat, non_null(:float)
    field :lon, non_null(:float)
    field :accuracy, non_null(:float)
  end

  payload_object(:user_location_update_payload, :boolean)

  object :user_queries do
    field :current_user, :user do
      resolve &User.get_current_user/3
    end

    field :user, :user do
      arg :id, non_null(:uuid)
      resolve &User.get_user/3
    end

    field :users, list_of(non_null(:user)) do
      arg :search_term, non_null(:string)
      arg :limit, :integer
      resolve &User.search_users/3
    end
  end

  object :user_mutations do
    field :user_update, type: :user_update_payload do
      arg :input, non_null(:user_update_input)
      resolve &User.update_user/3
      mutation_middleware
    end
  end

  object :location_mutations do
    field :user_location_update, type: :user_location_update_payload do
      arg :input, non_null(:user_location_update_input)
      resolve &User.update_location/3
      mutation_middleware
    end
  end
end
