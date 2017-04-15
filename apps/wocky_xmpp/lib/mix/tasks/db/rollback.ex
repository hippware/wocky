defmodule Mix.Tasks.Db.Rollback do
  use Mix.Task
  alias Mix.Wocky
  alias Schemata.Migrator

  @moduledoc "Rolls back one database migration"
  @shortdoc "Rolls back one database migration"

  def run(args) do
    Wocky.start_app(args)

    # Turn up the loglevel so we can see migration output
    Wocky.set_loglevel(:info)

    Wocky.info "Rolling back the database..."
    success =
      case Migrator.migrate(:down, 1) do
        {:ok, _} -> true
        {:error, _} -> false
      end
    Wocky.set_error_exit(!success)
  end
end
