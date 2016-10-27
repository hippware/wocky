defmodule Mix.Tasks.Db.Load do
  use Mix.Task
  alias Mix.Wocky
  alias Schemata.Schema

  @moduledoc "Loads the database schema from a file"
  @shortdoc "Loads the database schema from a file"

  def run(_) do
    Wocky.start_app
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
