defmodule Schemata.AddBotItemMigration do
  use Schemata.Migration, [
    authored_at: "2016-09-14T05:25:04Z",
    description: "Adds bot_items table"
  ]

  def up do
    create_table :bot_item, in: :wocky_db.local_keyspace,
      columns: [
        id:               :text,
        bot:              :timeuuid,
        published:        :timestamp,
        updated:          :timestamp,
        stanza:           :text
      ],
      primary_key: [:bot, :id]

    :ok
  end

  def down do
    drop :table, named: :bot_item, in: :wocky_db.local_keyspace
  end
end
