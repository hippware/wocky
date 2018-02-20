defmodule WockyAPI.Schema do
  @moduledoc "GraphQL schema for the Wocky API"

  use Absinthe.Schema

  alias WockyAPI.UserResolver

  object :current_user do
    field :profile, non_null(:profile) do
      resolve &UserResolver.get_profile/3
    end

    field :contacts_connection, :user_contacts_connection do
      arg :first, :integer
      arg :after, :string
      arg :last, :integer
      arg :before, :string
      arg :relationship, :user_contact_relationship

      resolve &UserResolver.get_contacts/3
    end

    field :home_stream_connection, :user_home_stream_connection do
      arg :first, :integer
      arg :after, :string
      arg :last, :integer
      arg :before, :string

      resolve &UserResolver.get_home_stream/3
    end

    field :conversations_connection, :user_conversations_connection do
      arg :first, :integer
      arg :after, :string
      arg :last, :integer
      arg :before, :string

      resolve &UserResolver.get_conversations/3
    end

    field :bots_connection, :user_bots_connection do
      arg :first, :integer
      arg :after, :string
      arg :last, :integer
      arg :before, :string
      arg :relationship, :user_bot_relationship

      resolve &UserResolver.get_bots/3
    end
  end

  object :profile do
    field :id, non_null(:id)
    field :server, non_null(:string)
    field :external_id, :string
    field :phone_number, :string
    field :email, :string
    field :handle, :string
    field :avatar, :string
    field :first_name, :string
    field :last_name, :string
    field :tagline, :string
    field :roles, non_null(list_of(non_null(:string)))
    field :bot_count, :integer
    field :follower_count, :integer
    field :following_count, :integer
  end

  object :page_info do
    field :has_next_page, non_null(:boolean)
    field :has_previous_page, non_null(:boolean)
  end

  enum :user_contact_relationship do
    value :following
    value :follower
    value :friend
  end

  object :user_contacts_connection do
    field :page_info, non_null(:page_info)
    field :edges, list_of(:user_contacts_edge)
  end

  object :user_contacts_edge do
    field :cursor, non_null(:string)
    field :relationship, non_null(:user_contact_relationship)
    field :node, non_null(:profile)
  end

  object :user_home_stream_connection do
    field :page_info, non_null(:page_info)
    field :edges, list_of(:user_home_stream_edge)
  end

  object :user_home_stream_edge do
    field :cursor, non_null(:string)
    field :node, non_null(:home_stream_item)
  end

  object :home_stream_item do
    field :id, non_null(:id)
    # ...
  end

  object :user_conversations_connection do
    field :page_info, non_null(:page_info)
    field :edges, list_of(:user_conversations_edge)
  end

  object :user_conversations_edge do
    field :cursor, non_null(:string)
    field :node, non_null(:conversation)
  end

  object :conversation do
    field :id, non_null(:id)
    # ...
  end

  enum :user_bot_relationship do
    value :owned
    value :shared
    value :subscribed
  end

  object :user_bots_connection do
    field :total_count, non_null(:integer)
    field :page_info, non_null(:page_info)
    field :edges, list_of(:user_bots_edge)
  end

  object :user_bots_edge do
    field :cursor, non_null(:string)
    field :relationship, non_null(:user_bot_relationship)
    field :node, non_null(:bot)
  end

  object :bot do
    field :id, non_null(:id)
    field :server, non_null(:string)
    field :title, non_null(:string)
    field :lat, non_null(:float)
    field :lon, non_null(:float)
    field :radius, non_null(:float)
    field :description, :string
    field :shortname, :string
    field :image, :string
    field :type, :string
    field :address, :string
    field :address_data, :string
    field :public, non_null(:boolean)
  end

  query do
    field :current_user, non_null(:current_user) do
      resolve fn _, _, _ -> {:ok, %{}} end
    end

    field :users, non_null(:profile) do
      arg :id, non_null(:string)

      resolve &UserResolver.get_user/3
    end
  end

  input_object :update_profile_input do
    field :client_mutation_id, :string
    field :handle, :string
    field :avatar, :string
    field :first_name, :string
    field :last_name, :string
    field :email, :string
    field :tagline, :string
  end

  object :update_profile_payload do
    field :client_mutation_id, :string
    field :profile, :profile
  end

  mutation do
    field :update_profile, :update_profile_payload do
      arg :input, non_null(:update_profile_input)

      resolve &UserResolver.update_profile/3
    end
  end
end
