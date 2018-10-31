defmodule WockyAPI.Resolvers.Block do
  @moduledoc "Resolver module for user blocking"

  import Ecto.Query

  alias Wocky.{Block, User}
  alias WockyAPI.Resolvers.Utils

  def get_blocks(%User{} = user, args, _info) do
    user.id
    |> Block.blocks_query()
    |> preload([:blockee])
    |> Utils.connection_from_query(
      user, [desc: :created_at], &map_to_block_user/1, args)
  end

  def block(_root, args, %{context: %{current_user: blocker}}),
    do: do_action(&Block.block/2, args, blocker)

  def unblock(_root, args, %{context: %{current_user: blocker}}),
    do: do_action(&Block.unblock/2, args, blocker)

  defp do_action(fun, args, blocker) do
    with %User{} = user <- User.get_user(args[:input][:user_id]) do
      fun.(blocker, user)
      {:ok, true}
    else
      _ -> {:error, "Invalid user"}
    end
  end

  def map_to_block_user(%Block{blockee: blockee, created_at: created_at}) do
    %{
      user_id: blockee.id,
      handle: blockee.handle,
      created_at: created_at
    }
  end
end
