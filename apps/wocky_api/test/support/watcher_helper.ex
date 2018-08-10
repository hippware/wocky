defmodule WockyAPI.WatcherHelper do
  @moduledoc """
  Helper functions for tests that require DB watcher functionality
  """

  use ExUnit.CaseTemplate

  @doc """
  This enables the DB watcher for the set of tests in which it's called.
  It should be placed in the `setup` section.

  NOTE: This will remove all DB data that is created in higher-level `setup`
  functions. You will need to recreate the data after calling this function.

  NOTE: I haven't been able to get SQL VIEW virtual tables to function properly
  in this mode. I have no idea why.
  """
  def require_watcher(_) do
    Wocky.Watcher.Client.clear_all_subscriptions()
    Wocky.Callbacks.register()
    WockyAPI.Callbacks.register()
    Ecto.Adapters.SQL.Sandbox.mode(Wocky.Repo, :auto)
    Application.start(:wocky_db_watcher)

    on_exit(fn ->
      Application.stop(:wocky_db_watcher)
      Wocky.Repo.delete_all(Wocky.User)
    end)
  end
end
