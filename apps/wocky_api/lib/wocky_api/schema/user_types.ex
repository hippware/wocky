defmodule WockyAPI.Schema.UserTypes do
  @moduledoc """
  Absinthe types for wocky user
  """

  use Absinthe.Schema.Notation
  use Absinthe.Relay.Schema.Notation, :modern

  import Kronky.Payload

  alias WockyAPI.BotResolver
  alias WockyAPI.UserResolver
  alias WockyAPI.UtilResolver

  object :user do
    field :id, non_null(:uuid)
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
      arg :relationship, :user_bot_relationship
      arg :id, :uuid
      resolve &BotResolver.get_bots/3
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
    field :total_count, non_null(:integer) do
      resolve &UtilResolver.get_count/3
    end
    edge do
      field :relationship, :user_contact_relationship do
        resolve &UserResolver.get_contact_relationship/3
      end
    end
  end

  object :home_stream_item do
    field :id, non_null(:string)
    # ...
  end

  object :location do
    field :lat, non_null(:float)
    field :lon, non_null(:float)
    field :accuracy, non_null(:float)
  end

  connection :home_stream, node_type: :home_stream_item do
    field :total_count, non_null(:integer) do
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
    field :total_count, non_null(:integer) do
      resolve &UtilResolver.get_count/3
    end
    edge do
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

  input_object :set_location_params do
    field :resource, non_null(:string)
    field :lat, non_null(:float)
    field :lon, non_null(:float)
    field :accuracy, non_null(:float)
  end

  payload_object(:location_payload, :location)


  object :user_queries do
    field :current_user, non_null(:user) do
      resolve &UserResolver.get_current_user/3
    end

    field :user, :user do
      arg :id, non_null(:uuid)
      resolve &UserResolver.get_user/3
    end
  end

  object :user_mutations do
    field :update_user, type: :user_payload do
      arg :user, non_null(:update_user_params)
      resolve &UserResolver.update_user/3
      middleware &UtilResolver.fix_changeset/2
      middleware &build_payload/2
    end
  end

  object :location_mutations do
    field :set_location, type: :location_payload do
      arg :location, non_null(:set_location_params)
      resolve &UserResolver.set_location/3
      middleware &UtilResolver.fix_changeset/2
      middleware &build_payload/2
    end
  end
end
