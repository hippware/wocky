defmodule WockyAPI.Resolvers.Auth do
  @moduledoc "GraphQL resolver for authentication"

  alias Wocky.Account

  def authenticate(_root, %{user: user_id, token: token}, _info) do
    case Account.authenticate(:token, "", {user_id, token}) do
      {:ok, {user, _}} ->
        {:ok, user}

      {:error, _} ->
        {:error, "invalid user ID / token combination"}
    end
  end
end
