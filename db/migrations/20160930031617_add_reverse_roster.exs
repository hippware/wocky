defmodule Schemata.AddReverseRosterMigration do
  use Schemata.Migration, [
    authored_at: "2016-09-30T03:16:17Z",
    description: "Add reverse_roster view"
  ]

  def up do
    create_view :reverse_roster,
      from: :roster, in: :wocky_db.shared_keyspace,
      columns: [:user, :server, :contact_jid],
      primary_key: [:contact_jid, :user]
  end

  def down do
    drop :materialized_view, named: :reverse_roster, in: :wocky_db.shared_keyspace
  end
end
