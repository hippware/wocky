defmodule WockyAPI.Test.Context do
  @moduledoc """
  CommonGraphQLClient context module providing top level interfaces for
  various wocky GraphQL operations
  """

  use CommonGraphQLClient.Context,
    otp_app: :wocky_api

  def authenticate(token), do: mutate(:authenticate, %{token: token})

  def user_delete, do: mutate(:user_delete)

  # Extending CommonGraphQLClient.Context:
  def mutate(term, variables \\ %{}), do: client().mutate(term, variables)
  def mutate!(term, variables \\ %{}), do: client().mutate!(term, variables)
end
