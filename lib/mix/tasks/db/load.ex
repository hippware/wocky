defmodule Mix.Tasks.Db.Load do
  use Mix.Task
  alias Mix.Wocky
  alias Schemata.Schema

  @moduledoc "Loads the database schema from a file"
  @shortdoc "Loads the database schema from a file"

  def run(args) do
    Wocky.start_app(args)

    {opts, _, _} = OptionParser.parse args, switches: [reset: :boolean]
    if opts[:reset], do: Mix.Task.run "db.reset", args

    success =
      case Schema.create_keyspace(:wocky_db.shared_keyspace) do
        :ok ->
          case Schema.create_keyspace(:wocky_db.local_keyspace) do
            :ok -> true
            {:error, _} -> false
          end

        {:error, _} -> false
      end
    Wocky.set_error_exit(!success)
  end
end
