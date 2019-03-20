defmodule Wocky.WatcherCase do
  @moduledoc false

  use ExUnit.CaseTemplate, async: false

  import DawdleDB.TestHelper

  using do
    quote do
      alias Wocky.Repo

      import Ecto
      import Ecto.Changeset
      import Ecto.Query
      import Eventually
      import unquote(__MODULE__)
    end
  end

  setup_all do
    Dawdle.Client.clear_all_handlers()

    # Give any DB notifications still in the system from previous tests
    # a grace period to finish up before we start the watcher
    # Process.sleep(500)

    Wocky.Callbacks.register()
    start_watcher(Wocky.Repo)

    # Because we can't use the Sandbox in its :manual mode (because it doesn't
    # cause the NOTIFY actions in the DB to fire) we have to do our own cleanup
    on_exit(fn ->
      Wocky.Repo.delete_all(Wocky.User)
    end)
  end
end
