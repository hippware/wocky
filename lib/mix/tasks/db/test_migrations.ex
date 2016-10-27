defmodule Mix.Tasks.Db.TestMigrations do
  use Mix.Task
  alias Mix.Wocky
  alias Porcelain.Result

  @moduledoc """
  Test the migrations to ensure they are consistent with the schema file.
  """
  @shortdoc "Test the migrations"

  @mig_out_file "migration_dump.cql"
  @schema_out_file "schema_dump.cql"

  def run(_) do
    shell = Mix.shell

    # Load the database from schema.exs
    shell.cmd("mix db.load --reset")

    # Dump the schema
    shell.cmd("mix db.dump #{@schema_out_file}")

    # Load the database from migrations
    shell.cmd("mix db.migrate --reset")

    # Dump the schema
    shell.cmd("mix db.dump #{@mig_out_file}")

    # Compare the schema dumps
    Application.ensure_started(:porcelain)
    %Result{out: output, status: status} =
      Porcelain.exec("diff", [@schema_out_file, @mig_out_file])

    if status == 0 do
      shell.info "Schemas match."
    else
      shell.error "Schemas differ!"
      shell.error output
      Wocky.set_error_exit
    end
  end
end
