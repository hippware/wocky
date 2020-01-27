defmodule WockyAPI.Test.Context do
  @moduledoc """
  CommonGraphQLClient context module providing top level interfaces for
  various wocky GraphQL operations
  """

  use CommonGraphQLClient.Context,
    otp_app: :wocky_api

  @spec authenticate(binary()) :: {:ok, map()} | {:error, any()}
  def authenticate(token), do: mutate(:authenticate, %{token: token})

  @spec user_delete :: {:ok, map()} | {:error, any()}
  def user_delete, do: mutate(:user_delete)

  # Extending CommonGraphQLClient.Context:
  @spec mutate(atom(), map()) :: {:ok, map()} | {:error, any()}
  def mutate(term, variables \\ %{}), do: client().mutate(term, variables)

  @spec mutate!(atom(), map()) :: map() | no_return()
  def mutate!(term, variables \\ %{}), do: client().mutate!(term, variables)
end
