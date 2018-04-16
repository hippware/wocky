defmodule WockyAPI.GraphQLHelper do
  @moduledoc """
  Helper functions for Wocky API GraphQL tests
  """

  import ExUnit.Assertions

  require ExUnit.Assertions

  def run_query(query, user \\ nil, variables \\ %{}) do
    assert {:ok, %{data: data}} =
             Absinthe.run(
               query,
               WockyAPI.Schema,
               variables: variables,
               context: %{current_user: user}
             )

    data
  end

  def assert_data(actual, expected) do
    assert actual == expected
  end
end
