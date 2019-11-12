defmodule WockyAPI.Resolvers.UserInvite do
  @moduledoc """
  GraphQL resolver for invitations to join the service
  """

  alias Wocky.UserInvite

  # -------------------------------------------------------------------
  # Mutations

  def user_invite_send(%{input: input}, %{context: %{current_user: user}}) do
    {:ok, UserInvite.send(input[:phone_number], input[:share_type], user)}
  end

  def user_invite_redeem_code(%{input: input}, %{context: %{current_user: user}}) do
    share_type = input[:share_type] || :disabled
    result = UserInvite.redeem_code(user, input[:code], share_type)
    {:ok, %{successful: result, result: result}}
  end

  def user_invite_make_code(_args, %{context: %{current_user: user}}) do
    {:ok, code} = UserInvite.make_code(user)
    {:ok, %{successful: true, result: code}}
  end

  def friend_bulk_invite(%{input: input}, %{context: %{current_user: user}}) do
    {:ok, UserInvite.send_multi(input[:phone_numbers], user)}
  end
end
