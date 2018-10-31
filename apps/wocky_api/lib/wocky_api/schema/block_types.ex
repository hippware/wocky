defmodule WockyAPI.Schema.BlockTypes do
  @moduledoc """
  Absinthe types to handle contact (un)blocking
  """

  use WockyAPI.Schema.Notation
  use Absinthe.Ecto, repo: Wocky.Repo

  import Kronky.Payload

  alias WockyAPI.Resolvers.Block

  @desc "A blocked user"
  object :block do
    @desc "The blocked user's ID"
    field :user_id, non_null(:uuid)

    @desc "The blocked user's handle"
    field :handle, non_null(:string)

    @desc "When the user was blocked"
    field :created_at, non_null(:datetime)
  end

  connection :blocks, node_type: :block do
    total_count_field()

    edge do
    end
  end


  input_object :user_block_input do
    @desc "The ID of the user to block"
    field :user_id, non_null(:uuid)
  end

  input_object :user_unblock_input do
    @desc "The ID of the user to unblock"
    field :user_id, non_null(:uuid)
  end

  payload_object(:user_block_payload, :boolean)
  payload_object(:user_unblock_payload, :boolean)

  object :block_mutations do
    @desc "Block a user"
    field :user_block, type: :user_block_payload do
      arg :input, non_null(:user_block_input)
      resolve &Block.block/3
      changeset_mutation_middleware()
    end

    @desc "Unblock a blocked user"
    field :user_unblock, type: :user_unblock_payload do
      arg :input, non_null(:user_unblock_input)
      resolve &Block.unblock/3
      changeset_mutation_middleware()
    end
  end
end
