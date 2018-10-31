defmodule Wocky.WatcherHelper do
  @moduledoc """
  Module to set up the db watcher/callback system for test cases that require it
  """

  alias Ecto.Adapters.SQL.Sandbox, as: SQLSandbox
  alias Wocky.Callbacks
  alias Wocky.Push.Sandbox, as: PushSandbox
  alias Wocky.Repo
  alias Wocky.User
  alias Wocky.Watcher.Client, as: WatcherClient

  defmacro __using__(_) do
    quote do
      use ExUnit.Case, async: false

      import unquote(__MODULE__)
      import Wocky.Eventually

      setup_all do
        WatcherClient.clear_all_subscriptions()
        SQLSandbox.mode(Wocky.Repo, :auto)
        # Give any DB notifications still in the system from previous tests
        # a grace period to finish up before we start the watcher
        Process.sleep(500)
        Callbacks.register()
        Application.start(:wocky_db_watcher)
        WatcherClient.start_link()

        # Because we can't use the Sandbox in its :manual mode (because it doesn't
        # cause the NOTIFY actions in the DB to fire) we have to do our own cleanup
        on_exit(fn ->
          Application.stop(:wocky_db_watcher)
          Repo.delete_all(User)
        end)
      end
    end
  end

  def no_more_push_notifications do
    msgs = PushSandbox.wait_notifications(global: true)
    Enum.empty?(msgs)
  end

  def clear_expected_notifications(count) do
    result =
      length(
        PushSandbox.wait_notifications(global: true, timeout: 500, count: count)
      ) == count

    PushSandbox.clear_notifications(global: true)
    result
  end
end
