defmodule Wocky.WatcherHelper do
  @moduledoc """
  Module to set up the db watcher/callback system for test cases that require it
  """
  alias Wocky.Push.Sandbox

  defmacro __using__(_) do
    quote do
      use ExUnit.Case, async: false
      import unquote(__MODULE__)
      import Wocky.Eventually

      setup_all do
        Wocky.Watcher.Client.clear_all_subscriptions()
        Ecto.Adapters.SQL.Sandbox.mode(Wocky.Repo, :auto)
        # Give any DB notifications still in the system from previous tests
        # a grace period to finish up before we start the watcher
        :timer.sleep(500)
        Wocky.Callbacks.register()
        Application.start(:wocky_db_watcher)
        Wocky.Watcher.Client.start_link()

        # Because we can't use the Sandbox in its :manual mode (because it doesn't
        # cause the NOTIFY actions in the DB to fire) we have to do our own cleanup
        on_exit(fn ->
          Application.stop(:wocky_db_watcher)
          Wocky.Repo.delete_all(Wocky.User)
        end)
      end
    end
  end

  def no_more_push_notifications do
    msgs = Sandbox.wait_notifications(global: true)
    Enum.empty?(msgs)
  end

  def clear_expected_notifications(count) do
    result =
      length(
        Sandbox.wait_notifications(global: true, timeout: 500, count: count)
      ) == count

    Sandbox.clear_notifications(global: true)
    result
  end
end
