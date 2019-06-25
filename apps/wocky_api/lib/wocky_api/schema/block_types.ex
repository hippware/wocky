defmodule WockyAPI.Schema.BlockTypes do
  @moduledoc """
  Absinthe types to handle contact (un)blocking
  """

  use WockyAPI.Schema.Notation

  import AbsintheErrorPayload.Payload

  alias WockyAPI.Resolvers.Block
  alias WockyAPI.Resolvers.Media

  @desc "A block on a user"
  object :block do
    @desc "The blocked user"
    field :user, non_null(:blocked_user)

    @desc "When the user was blocked"
    field :created_at, non_null(:datetime)
  end

  @desc "A user that has been blocked by the requesting user"
  object :blocked_user do
    @desc "The user's ID"
    field :id, non_null(:uuid)

    @desc "The user's handle"
    field :handle, non_null(:string)

    @desc "The user's first name"
    field :first_name, :string

    @desc "The user's last name"
    field :last_name, :string

    @desc "The user's avatar"
    field :media, :media do
      resolve &Media.get_media/3
    end
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
