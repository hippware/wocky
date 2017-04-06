defmodule Schemata.RemoveFilePurposeMigration do
  use Schemata.Migration, [
    authored_at: "2016-09-30T05:52:25Z",
    description: "Remove TROS purpose field"
  ]

  def up do
    alter_table :media, in: :wocky_db.local_keyspace, drop: :purpose
    alter_table :tros_request, in: :wocky_db.local_keyspace, drop: :purpose
  end

  def down do
    alter_table :media, in: :wocky_db.local_keyspace,
      add: :purpose, type: :text
    alter_table :tros_request, in: :wocky_db.local_keyspace,
      add: :purpose, type: :text
  end
end
