defmodule WockyAPI.GraphQLHelper do
  @moduledoc """
  Helper functions for Wocky API GraphQL tests
  """

  def run_query(query, user \\ nil, variables \\ %{}) do
    Absinthe.run!(
      query,
      WockyAPI.Schema,
      variables: variables,
      context: build_context(user)
    )
  end

  defp build_context(%Wocky.User{} = user), do: %{current_user: user}
  defp build_context(_), do: %{}

  def has_data(result) do
    Map.has_key?(result, :data)
  end

  def has_errors(result) do
    Map.has_key?(result, :errors)
  end

  def error_count(result) do
    result |> Map.get(:errors, []) |> length()
  end

  def error_msg(result, idx \\ 0) do
    error =
      result
      |> Map.get(:errors, [])
      |> Enum.at(idx, %{})

    error[:message]
  end
end
