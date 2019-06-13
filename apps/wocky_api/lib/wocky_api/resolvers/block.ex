defmodule WockyAPI.Resolvers.Block do
  @moduledoc "Resolver module for user blocking"

  import Ecto.Query

  alias Wocky.Account
  alias Wocky.Account.User
  alias Wocky.Block
  alias WockyAPI.Resolvers.Utils

  def get_blocks(%User{} = user, args, _info) do
    user.id
    |> Block.blocks_query()
    |> preload([:blockee])
    |> Utils.connection_from_query(
      user,
      args,
      order_by: [desc: :created_at],
      postprocess: &map_to_block_user/1
    )
  end

  def block(_root, args, %{context: %{current_user: blocker}}),
    do: do_action(&Block.block/2, args, blocker)

  def unblock(_root, args, %{context: %{current_user: blocker}}),
    do: do_action(&Block.unblock/2, args, blocker)

  defp do_action(fun, args, blocker) do
    case Account.get_user(args[:input][:user_id]) do
      nil ->
        {:error, "Invalid user"}

      user ->
        fun.(blocker, user)
        {:ok, true}
    end
  end

  def map_to_block_user(%Block{blockee: blockee, created_at: created_at}) do
    %{
      user: blockee,
      created_at: created_at
    }
  end
end
