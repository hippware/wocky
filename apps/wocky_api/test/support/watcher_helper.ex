defmodule WockyAPI.WatcherHelper do
  @moduledoc """
  Helper functions for tests that require DB watcher functionality
  """

  import Dawdle.TestHelper
  import DawdleDB.TestHelper
  import ExUnit.Callbacks

  @doc """
  This enables the DB watcher for the set of tests in which it's called.
  It should be placed in the `setup` section.

  NOTE: This will remove all DB data that is created in higher-level `setup`
  functions. You will need to recreate the data after calling this function.

  NOTE: I haven't been able to get SQL VIEW virtual tables to function properly
  in this mode. I have no idea why.
  """
  @spec require_watcher(map()) :: :ok
  def require_watcher(_ \\ %{}) do
    start_watcher(Wocky.Repo)

    on_exit(fn ->
      clear_all_handlers()
      Wocky.Repo.delete_all(Wocky.Account.User)
    end)
  end
end
