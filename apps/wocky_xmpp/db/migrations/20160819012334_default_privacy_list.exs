defmodule Schemata.DefaultPrivacyListMigration do
  use Schemata.Migration, [
    authored_at: "2016-08-19T01:23:34Z",
    description: "Remove fields for default privacy list and ask message"
  ]

  def up do
    # We have to drop the view to delete the column
    drop :materialized_view, named: :roster_version, in: :wocky_db.local_keyspace
    alter_table :roster, in: :wocky_db.local_keyspace, drop: :ask_message
    create_view :roster_version,
      from: :roster, in: :wocky_db.local_keyspace,
      columns: :all,
      primary_key: [:user, :version, :contact_jid],
      order_by: [version: :asc]

    alter_table :privacy, in: :wocky_db.local_keyspace, drop: :default
  end

  def down do
    drop :materialized_view, named: :roster_version, in: :wocky_db.local_keyspace
    alter_table :roster, in: :wocky_db.local_keyspace,
      add: :ask_message, type: :text
    create_view :roster_version,
      from: :roster, in: :wocky_db.local_keyspace,
      columns: :all,
      primary_key: [:user, :version, :contact_jid],
      order_by: [version: :asc]

    alter_table :privacy, in: :wocky_db.local_keyspace,
      add: :default, type: :text
  end
end
