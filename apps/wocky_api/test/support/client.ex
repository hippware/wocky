defmodule WockyAPI.Test.Client do
  @moduledoc """
  CommonGraphQLClient client module providing implementation for wocky-specific
  GraphQL operations
  """

  use CommonGraphQLClient.Client,
    otp_app: :wocky_api,
    mod: WockyAPI.Test.Context

  alias WockyAPI.Test.Query.Auth
  alias WockyAPI.Test.Query.User

  defp handle(:mutate, :authenticate, variables) do
    post(Auth.authenticate(), variables)
  end

  defp handle(:mutate, :user_delete, variables) do
    post(User.user_delete(), variables)
  end

  # Extending CommonGraphQLClient.Client:
  @spec mutate(atom(), Keyword.t()) :: {:ok, map()} | {:error, any()}
  def mutate(term, variables), do: handle(:mutate, term, variables)

  @spec mutate!(atom(), Keyword.t()) :: map() | no_return()
  def mutate!(term, variables) do
    case mutate(term, variables) do
      {:ok, result} -> result
      {:error, errors} -> raise "#{inspect(errors)}"
    end
  end
end
