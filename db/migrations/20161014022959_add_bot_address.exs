defmodule Schemata.AddBotAddressMigration do
  use Schemata.Migration, [
    authored_at: "2016-10-14T02:29:59Z",
    description: "Add address field to bot"
  ]

  def up do
    alter_table :bot_item, in: :wocky_db.local_keyspace,
      add: :address, type: :text
  end

  def down do
    alter_table :bot_item, in: :wocky_db.local_keyspace, drop: :address
  end
end
