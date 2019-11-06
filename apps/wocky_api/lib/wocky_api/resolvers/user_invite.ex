defmodule WockyAPI.Resolvers.UserInvite do
  @moduledoc """
  GraphQL resolver for invitations to join the service
  """

  alias Wocky.UserInvite

  # -------------------------------------------------------------------
  # Mutations

  def user_invite_make_code(_args, %{context: %{current_user: user}}) do
    code = UserInvite.make_code(user)
    {:ok, %{successful: true, result: code}}
  end

  def user_invite_redeem_code(args, %{context: %{current_user: user}}) do
    result = UserInvite.redeem_code(user, args[:input][:code])
    {:ok, %{successful: result, result: result}}
  end

  def friend_bulk_invite(args, %{context: %{current_user: user}}) do
    {:ok, UserInvite.send(args[:input][:phone_numbers], user)}
  end
end
