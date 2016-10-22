defmodule Mix.Tasks.Db.Rollback do
  use Mix.Task
  alias Mix.Wocky
  alias Schemata.Migrator

  @moduledoc "Rolls back one database migration"
  @shortdoc "Rolls back one database migration"

  def run(_) do
    Wocky.start_app
    success =
      case Migrator.migrate(:down, 1) do
        {:ok, _} -> true
        {:error, _} -> false
      end
    Wocky.set_error_exit(!success)
  end
end
