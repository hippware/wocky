defmodule Wocky.WatcherCase do
  @moduledoc false

  use ExUnit.CaseTemplate, async: false

  import Dawdle.TestHelper
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
    start_watcher(Wocky.Repo)

    # Because we can't use the Sandbox in its :manual mode (because it doesn't
    # cause the NOTIFY actions in the DB to fire) we have to do our own cleanup
    on_exit(fn ->
      clear_all_handlers()
      Wocky.Repo.delete_all(Wocky.Account.User)
    end)
  end
end
