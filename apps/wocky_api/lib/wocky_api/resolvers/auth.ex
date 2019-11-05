defmodule WockyAPI.Resolvers.Auth do
  @moduledoc "GraphQL resolver for authentication"

  alias Wocky.Account

  # -------------------------------------------------------------------
  # Mutations

  def authenticate(_input, %{context: %{current_user: _}}) do
    {:error, "already authenticated"}
  end

  def authenticate(%{token: token}, _info) do
    case Account.authenticate(token) do
      {:ok, auth_data} ->
        {:ok, auth_data}

      {:error, _} ->
        {:error, "invalid user token"}
    end
  end
end
