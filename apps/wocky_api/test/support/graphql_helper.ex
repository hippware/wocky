defmodule WockyAPI.GraphQLHelper do
  @moduledoc """
  Helper functions for Wocky API GraphQL tests
  """

  def run_query(query, user \\ nil, variables \\ %{}) do
    Absinthe.run!(
      query,
      WockyAPI.Schema,
      variables: variables,
      context: %{current_user: user}
    )
  end

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
