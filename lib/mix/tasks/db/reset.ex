defmodule Mix.Tasks.Db.Reset do
  use Mix.Task
  alias Mix.Wocky
  alias Schemata.Schema
  alias Schemata.Migrator

  @moduledoc "Resets the database schema."
  @shortdoc "Resets the database schema."

  def run(_) do
    Wocky.start_app
    :ok = Schema.reset_keyspace(:wocky_db.shared_keyspace)
    :ok = Schema.reset_keyspace(:wocky_db.local_keyspace)
    :ok = Migrator.reset
  end
end
