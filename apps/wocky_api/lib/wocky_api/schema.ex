defmodule WockyAPI.Schema do
  @moduledoc "GraphQL schema for the Wocky API"

  use Absinthe.Schema
  use Absinthe.Relay.Schema, :modern

  alias WockyAPI.BotResolver
  alias WockyAPI.UserResolver

  object :user do
    field :id, non_null(:id)
    field :server, non_null(:string)
    field :handle, :string
    field :avatar, :string
    field :first_name, :string
    field :last_name, :string
    field :tagline, :string
    field :roles, non_null(list_of(non_null(:string)))

    connection field :owned_bots, node_type: :owned_bots do
      resolve &UserResolver.get_owned_bots/3
    end

    connection field :contacts, node_type: :contacts do
      resolve &UserResolver.get_contacts/3
    end
  end

  object :private_user_data do
    field :external_id, :string
    field :phone_number, :string
    field :email, :string
  end

  enum :user_bot_relationship do
    value :visible
    value :owned
    value :shared
    value :subscribed
    value :guest
    value :visitor
  end

  connection :owned_bots, node_type: :bot do
    field :total_count, :integer do
      resolve &UserResolver.get_owned_bots_total_count/3
    end
    edge do
      field :relationship, :user_bot_relationship do
        resolve fn _, _ -> {:ok, :owned} end
      end
    end
  end

  connection :bots, node_type: :bot do
    field :total_count, :integer do
      resolve &UserResolver.get_bots_total_count/3
    end
    edge do
      field :relationship, :user_bot_relationship do
        resolve &UserResolver.get_bot_relationship/3
      end
    end
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
  end

  enum :user_contact_relationship do
    value :following
    value :follower
    value :friend
  end

  connection :contacts, node_type: :user do
    field :total_count, :integer do
      resolve &UserResolver.get_contacts_total_count/3
    end
    field :follower_count, :integer do
      resolve &UserResolver.get_contacts_follower_count/3
    end
    field :following_count, :integer do
      resolve &UserResolver.get_contacts_following_count/3
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
    edge do
    end
  end

  object :conversation do
    field :id, non_null(:id)
    # ...
  end

  connection :conversations, node_type: :conversation do
    edge do
    end
  end

  query do
    field :current_user, non_null(:user) do
      resolve &UserResolver.get_current_user/3
    end

    field :private_user_data, non_null(:private_user_data) do
      resolve &UserResolver.get_private_user_data/3
    end

    connection field :bots, node_type: :bots do
      arg :relationship, :user_bot_relationship

      resolve &UserResolver.get_bots/3
    end

    connection field :home_stream, node_type: :home_stream do
      resolve &UserResolver.get_home_stream/3
    end

    connection field :conversations, node_type: :conversations do
      resolve &UserResolver.get_conversations/3
    end

    field :user, :user do
      arg :id, non_null(:string)

      resolve &UserResolver.get_user/3
    end

    field :public_bot, :bot do
      arg :id, non_null(:string)

      resolve &BotResolver.get_public_bot/3
    end
  end

  mutation do
    payload field :update_user do
      input do
        field :handle, :string
        field :avatar, :string
        field :first_name, :string
        field :last_name, :string
        field :email, :string
        field :tagline, :string
      end

      output do
        field :user, :user
        field :private_user_data, :private_user_data
      end

      resolve &UserResolver.update_user/3
    end
  end
end
