defmodule WockyAPI.Resolvers.Block do
  @moduledoc "Resolver module for user blocking"

  alias Wocky.Account
  alias Wocky.Account.User
  alias Wocky.Contacts
  alias WockyAPI.Resolvers.Utils

  # -------------------------------------------------------------------
  # Connections

  def get_blocks(%User{} = user, args, _info) do
    user
    |> Contacts.blocks_query()
    |> Utils.connection_from_query(
      user,
      args,
      order_by: [desc: :created_at],
      postprocess: &map_to_block_user/1
    )
  end

  # -------------------------------------------------------------------
  # Mutations

  def user_block(args, %{context: %{current_user: blocker}}),
    do: do_action(&Contacts.block/2, args, blocker)

  def user_unblock(args, %{context: %{current_user: blocker}}),
    do: do_action(&Contacts.unblock/2, args, blocker)

  defp do_action(fun, %{input: input}, blocker) do
    case Account.get_user(input[:user_id]) do
      nil ->
        {:error, "Invalid user"}

      user ->
        fun.(blocker, user)
        {:ok, true}
    end
  end

  def map_to_block_user(%{contact: blockee, created_at: created_at}) do
    %{
      user: blockee,
      created_at: created_at
    }
  end
end
