defmodule WockyAPI.GraphQLCase do
  @moduledoc """
  This module defines the test case to be used by
  tests that run GraphQL queries.

  Finally, if the test case interacts with the database,
  it cannot be async. For this reason, every test runs
  inside a transaction which is reset at the beginning
  of the test unless the test case is marked as async.
  """

  use ExUnit.CaseTemplate

  alias Ecto.Adapters.SQL.Sandbox

  using do
    quote do
      # Import conveniences for testing with GraphQL
      import WockyAPI.GraphQLHelper

      # Import conveniences for testing with the watcher enabled
      import WockyAPI.WatcherHelper
    end
  end

  setup tags do
    :ok = Sandbox.checkout(Wocky.Repo)

    unless tags[:async] do
      Sandbox.mode(Wocky.Repo, {:shared, self()})
    end

    {:ok, []}
  end
end
