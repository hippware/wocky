defmodule WockyAPI.Schema.CollectionTypes do
  @moduledoc """
  Absinthe types for wocky collections
  """

  use WockyAPI.Schema.Notation

  import Kronky.Payload

  alias WockyAPI.Resolvers.Collection
  alias WockyAPI.Resolvers.User

  @desc "A collection of bots"
  object :collection do
    @desc "The collection's unique ID"
    field :id, non_null(:aint)
    @desc "The collection's title"
    field :title, non_null(:string)
    @desc "The collection's owner"
    field :owner, non_null(:user), do: resolve(&User.get_object_owner/3)

    @desc "The set of bots comprising the collection"
    connection field :bots, node_type: :collection_bots do
      connection_complexity
      resolve &Collection.get_bots/3
    end

    @desc "Subscribers to the collection"
    connection field :subscribers, node_type: :collection_subscribers do
      connection_complexity
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
    @desc "The new collection's title"
    field :title, non_null(:string)
  end

  payload_object :collection_create_payload, :collection

  input_object :collection_update_input do
    @desc "ID of the collection to be updated"
    field :id, non_null(:aint)
    @desc "New title for the collection"
    field :title, non_null(:string)
  end

  payload_object :collection_update_payload, :collection

  object :collection_queries do
    @desc "Retrieve a single collection by ID"
    field :collection, :collection do
      @desc "The ID of the collection being requested"
      arg :id, non_null(:aint)
      resolve &Collection.get_collection/3
    end
  end

  object :collection_mutations do
    @desc "Create a new collection"
    field :collection_create, :collection_update_payload do
      arg :input, non_null(:collection_create_input)
      resolve &Collection.create/3
      changeset_mutation_middleware
    end

    @desc "Update a collection"
    field :collection_update, :collection_update_payload do
      arg :input, non_null(:collection_update_input)
      resolve &Collection.update/3
      changeset_mutation_middleware
    end

    @desc "Delete a collection"
    payload field :collection_delete do
      input do
        @desc "The ID of the collection to delete"
        field :id, non_null(:aint)
      end

      output do
        field :result, :boolean
      end

      resolve &Collection.delete/3
    end

    @desc "Subscribe the current user to a collection"
    payload field :collection_subscribe do
      input do
        @desc "The ID of the collection to which to subscribe"
        field :id, non_null(:aint)
      end

      output do
        field :result, :boolean
      end

      resolve &Collection.subscribe/3
    end

    @desc "Unsubscribe the current user from a collection"
    payload field :collection_unsubscribe do
      input do
        @desc "The ID of the collection from which to subscribe"
        field :id, non_null(:aint)
      end

      output do
        field :result, :boolean
      end

      resolve &Collection.unsubscribe/3
    end

    @desc "Add a bot to an owned collection"
    payload field :collection_add_bot do
      input do
        @desc "The ID of the collection being added to"
        field :id, non_null(:aint)
        @desc "The ID of the bot to add"
        field :bot_id, non_null(:uuid)
      end

      output do
        field :result, :boolean
      end

      resolve &Collection.add_bot/3
    end

    @desc "Remove a bot from an owned collection"
    payload field :collection_remove_bot do
      input do
        @desc "The ID of the collection being removed from"
        field :id, non_null(:aint)
        @desc "The ID of the bot to remove"
        field :bot_id, non_null(:uuid)
      end

      output do
        field :result, :boolean
      end

      resolve &Collection.remove_bot/3
    end

    @desc "Share a collection to another user"
    payload field :collection_share do
      input do
        @desc "The ID of the collection to share"
        field :id, non_null(:aint)
        @desc "The ID of the user to whom to share"
        field :user_id, non_null(:uuid)
        @desc "The message accompanying the share"
        field :message, :string
      end

      output do
        field :result, :boolean
      end

      resolve &Collection.share/3
    end
  end
end
