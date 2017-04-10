defmodule Mix.Tasks.Db.Dump do
  use Mix.Task
  alias Mix.Wocky

  @moduledoc """
  Dump the current database schema in CQL format.
  """
  @shortdoc "dump the database schema"

  def run(args) do
    Wocky.start_app(args)

    output = case args do
      [file] ->
        _ = File.rm_rf!(file)
        {:append, file}

      _else ->
        IO.binstream(:standard_io, :line)
    end

    _ = dump_keyspace(:wocky_db.shared_keyspace, output)
    _ = dump_keyspace(:wocky_db.local_keyspace, output)
  end

  defp dump_keyspace(ks, output) do
    Wocky.info "Dumping keyspace #{ks}..."
    Porcelain.exec("cqlsh", ["-e", "DESCRIBE KEYSPACE #{ks}"], out: output)
  end
end
