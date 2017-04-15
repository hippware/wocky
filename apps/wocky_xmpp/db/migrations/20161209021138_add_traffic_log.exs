defmodule Schemata.AddTrafficLogMigration do
  use Schemata.Migration, [
    authored_at: "2016-12-09T02:11:38Z",
    description: "Add traffic log table"
  ]

  def up do
    create_table :traffic_log, in: :wocky_db.shared_keyspace,
      columns: [
        user:      :text,
        resource:  :text,
        timestamp: :timestamp,
        ip:        :text,
        incoming:  :boolean,
        server:    :text,
        packet:    :text
      ],
      primary_key: [:user, :timestamp],
      order_by: [timestamp: :asc]
  end

  def down do
    drop :table, named: :traffic_log, in: :wocky_db.shared_keyspace
  end
end
