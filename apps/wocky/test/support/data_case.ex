defmodule Wocky.DataCase do
  @moduledoc """
  This module defines the setup for tests requiring
  access to the application's data layer.

  You may define functions here to be used as helpers in
  your tests.

  Finally, if the test case interacts with the database,
  it cannot be async. For this reason, every test runs
  inside a transaction which is reset at the beginning
  of the test unless the test case is marked as async.
  """

  use ExUnit.CaseTemplate

  alias Ecto.Adapters.SQL.Sandbox

  using do
    quote do
      alias Wocky.Repo
      alias Wocky.Repo.Factory

      import Ecto
      import Ecto.Changeset
      import Ecto.Query
      import Wocky.DataCase
      import Wocky.Errors, only: [errors_on: 1]
    end
  end

  setup tags do
    :ok = Sandbox.checkout(Wocky.Repo)

    unless tags[:async] do
      Sandbox.mode(Wocky.Repo, {:shared, self()})
    end

    # Make sure that there are no Dawdle handlers registered to avoid
    # weird test failures.
    Dawdle.Client.clear_all_handlers()

    :ok
  end
end
