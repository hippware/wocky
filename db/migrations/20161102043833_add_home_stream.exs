defmodule Schemata.AddHomeStreamMigration do
  use Schemata.Migration, [
    authored_at: "2016-11-02T04:38:33Z",
    description: "Add home stream table"
  ]

  def up do
    create_table :home_stream, in: :wocky_db.local_keyspace,
      columns: [
        user:     :text,
        server:   :text,
        id:       :text,
        version:  :timeuuid,
        from_id:  :text,
        stanza:   :text,
        deleted:  :boolean
      ],
      primary_key: [[:user, :server], :version],
      order_by: [version: :asc]

    create_view :home_stream_item, in: :wocky_db.local_keyspace,
      from:     :home_stream,
      columns:  [:user, :server, :id, :version],
      primary_key: [:user, :server, :id, :version]
  end

  def down do
    drop :materialized_view, named: :home_stream_item, in: :wocky_db.local_keyspace

    drop :table, named: :home_stream, in: :wocky_db.local_keyspace
  end
end
