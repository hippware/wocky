defmodule WockyAPI.Resolvers.Auth do
  @moduledoc "GraphQL resolver for authentication"

  alias Wocky.Account

  def authenticate(_root, %{user: user_id, token: token}, _info) do
    do_authenticate(:token, {user_id, token})
  end

  def authenticate(_root, %{token: token}, _info) do
    do_authenticate(:client_jwt, token)
  end

  defp do_authenticate(method, creds) do
    case Account.authenticate(method, "", creds) do
      {:ok, {user, _}} ->
        {:ok, %{user: user}}

      {:error, _} ->
        {:error, "invalid user ID / token combination"}
    end
  end
end
