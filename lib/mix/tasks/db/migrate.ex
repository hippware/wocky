defmodule Mix.Tasks.Db.Migrate do
  use Mix.Task
  alias Mix.Wocky
  alias Schemata.Migrator

  @moduledoc "Runs the database migrations"
  @shortdoc "Runs the database migrations"

  def run(args) do
    Wocky.start_app(args)

    # Turn up the loglevel so we can see migration output
    Wocky.set_loglevel(:info)

    {opts, _, _} = OptionParser.parse args, switches: [reset: :boolean]
    if opts[:reset], do: Mix.Task.run "db.reset", args

    success =
      case Migrator.migrate(:up) do
        {:ok, _} -> true
        {:error, _} -> false
      end
    Wocky.set_error_exit(!success)
  end
end
