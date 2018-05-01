defmodule WockyAPI.Schema.CollectionTypes do
  @moduledoc """
  Absinthe types for wocky collections
  """

  use WockyAPI.Schema.Notation

  import Kronky.Payload

  alias WockyAPI.Resolvers.Collection
  alias WockyAPI.Resolvers.User

  object :collection do
    field :id, non_null(:aint)
    field :title, non_null(:string)
    field :owner, non_null(:user), do: resolve(&User.get_object_owner/3)

    connection field :bots, node_type: :collection_bots do
      resolve &Collection.get_bots/3
    end

    connection field :subscribers, node_type: :collection_subscribers do
      resolve &Collection.get_subscribers/3
    end
  end

  connection :collections, node_type: :collection do
    total_count_field

    edge do
    end
  end

  connection :collection_bots, node_type: :bot do
    total_count_field

    edge do
    end
  end

  connection :collection_subscribers, node_type: :user do
    total_count_field

    edge do
    end
  end

  input_object :collection_create_input do
    field :title, non_null(:string)
  end

  payload_object :collection_create_payload, :collection

  input_object :collection_update_input do
    field :id, non_null(:aint)
    field :title, non_null(:string)
  end

  payload_object :collection_update_payload, :collection

  object :collection_queries do
    field :collection, :collection do
      arg :id, non_null(:aint)
      resolve &Collection.get_collection/3
    end
  end

  object :collection_mutations do
    field :collection_create, :collection_update_payload do
      arg :input, non_null(:collection_create_input)
      resolve &Collection.create/3
      mutation_middleware
    end

    field :collection_update, :collection_update_payload do
      arg :input, non_null(:collection_update_input)
      resolve &Collection.update/3
      mutation_middleware
    end

    payload field :collection_delete do
      input do
        field :id, non_null(:aint)
      end

      output do
        field :result, :boolean
      end

      resolve &Collection.delete/3
    end

    payload field :collection_subscribe do
      input do
        field :id, non_null(:aint)
      end

      output do
        field :result, :boolean
      end

      resolve &Collection.subscribe/3
    end

    payload field :collection_unsubscribe do
      input do
        field :id, non_null(:aint)
      end

      output do
        field :result, :boolean
      end

      resolve &Collection.unsubscribe/3
    end

    payload field :collection_add_bot do
      input do
        field :id, non_null(:aint)
        field :bot_id, non_null(:uuid)
      end

      output do
        field :result, :boolean
      end

      resolve &Collection.add_bot/3
    end

    payload field :collection_remove_bot do
      input do
        field :id, non_null(:aint)
        field :bot_id, non_null(:uuid)
      end

      output do
        field :result, :boolean
      end

      resolve &Collection.remove_bot/3
    end

    payload field :collection_share do
      input do
        field :id, non_null(:aint)
        field :user_id, non_null(:uuid)
        field :message, :string
      end

      output do
        field :result, :boolean
      end

      resolve &Collection.share/3
    end
  end
end
