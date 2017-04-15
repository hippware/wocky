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
    Wocky.info "Comparing schemas..."
    shell = Mix.shell

    # Load the database from schema.exs
    shell.cmd("#{mix_cmd()} db.load --reset")

    # Dump the schema
    shell.cmd("#{mix_cmd()} db.dump #{@schema_out_file}")

    # Load the database from migrations
    shell.cmd("#{mix_cmd()} db.migrate --reset")

    # Dump the schema
    shell.cmd("#{mix_cmd()} db.dump #{@mig_out_file}")

    # Compare the schema dumps
    :ok = Application.ensure_started(:porcelain)
    %Result{out: output, status: status} =
      Porcelain.exec("diff", [@schema_out_file, @mig_out_file])

    if status == 0 do
      Wocky.info "Schemas match."
    else
      Wocky.error "Schemas differ!"
      Wocky.error output
      Wocky.set_error_exit
    end
  end

  defp mix_cmd, do: "MIX_ENV=#{Mix.env} mix"
end
