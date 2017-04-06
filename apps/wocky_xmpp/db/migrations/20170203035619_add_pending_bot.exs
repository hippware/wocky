defmodule Schemata.AddPendingBotMigration do
  use Schemata.Migration, [
    authored_at: "2017-02-03T03:56:19Z",
    description: "Add pending_bot table"
  ]

  def up do
    create_table :pending_bot, in: :wocky_db.shared_keyspace,
      columns: [
        id:    :text,
        owner: :text
      ],
      primary_key: [:id, :owner]
  end

  def down do
    drop :table, named: :pending_bot, in: :wocky_db.shared_keyspace
  end
end
