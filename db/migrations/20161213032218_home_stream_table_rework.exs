defmodule Schemata.HomeStreamTableReworkMigration do
  use Schemata.Migration, [
    authored_at: "2016-12-13T03:22:18Z",
    description: "Rework home_stream table"
  ]

  def up do
    drop :materialized_view, named: :home_stream_item, in: :wocky_db.local_keyspace

    create_new_table(:temp_home_stream)
    copy_table(:home_stream, :temp_home_stream)
    drop_table(:home_stream)
    create_new_table(:home_stream)
    copy_table(:temp_home_stream, :home_stream)
    drop_table(:temp_home_stream)

    create_view :home_stream_chronology, in: :wocky_db.local_keyspace,
      from:     :home_stream,
      columns:  :all,
      primary_key: [[:user, :server], :version, :id],
      order_by: [version: :asc]
  end

  def down do
    drop :materialized_view, named: :home_stream_chronology, in: :wocky_db.local_keyspace

    create_old_table(:temp_home_stream)
    copy_table(:home_stream, :temp_home_stream)
    drop_table(:home_stream)
    create_old_table(:home_stream)
    copy_table(:temp_home_stream, :home_stream)
    drop_table(:temp_home_stream)

    create_view :home_stream_item, in: :wocky_db.local_keyspace,
      from:     :home_stream,
      columns:  [:user, :server, :id, :version],
      primary_key: [:user, :server, :id, :version]
  end

  defp create_old_table(name) do
    create_table name, in: :wocky_db.local_keyspace,
      columns: columns,
      primary_key: [[:user, :server], :version],
      order_by: [version: :asc]
  end

  defp create_new_table(name) do
    create_table name, in: :wocky_db.local_keyspace,
      columns: columns,
      primary_key: [[:user, :server], :id]
  end

  defp columns() do
    [
      user:     :text,
      server:   :text,
      id:       :text,
      version:  :timeuuid,
      from_id:  :text,
      stanza:   :text,
      deleted:  :boolean
    ]
  end

  defp drop_table(name) do
    drop :table, named: name, in: :wocky_db.local_keyspace
  end

  defp copy_table(from, to) do
    select(:all, from: from, in: :wocky_db.local_keyspace)
    |> Enum.each(fn(v) ->
      insert into: to, in: :wocky_db.local_keyspace,
      values: v
    end)
  end
end
