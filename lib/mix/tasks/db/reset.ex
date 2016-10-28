defmodule Mix.Tasks.Db.Reset do
  use Mix.Task
  alias Mix.Wocky
  alias Schemata.Schema
  alias Schemata.Migrator

  @moduledoc "Resets the database schema."
  @shortdoc "Resets the database schema."

  def run(args) do
    Wocky.start_app(args)

    Wocky.info "Resetting keyspace #{:wocky_db.shared_keyspace}..."
    :ok = Schema.reset_keyspace(:wocky_db.shared_keyspace)

    Wocky.info "Resetting keyspace #{:wocky_db.local_keyspace}..."
    :ok = Schema.reset_keyspace(:wocky_db.local_keyspace)

    Wocky.info "Resetting migrations..."
    :ok = Migrator.reset
  end
end
