defmodule Schemata.AddBotImageTypeMigration do
  use Schemata.Migration, [
    authored_at: "2016-09-30T03:55:40Z",
    description: "Add image and type fields to bot"
  ]

  def up do
    alter_table :bot, in: :wocky_db.shared_keyspace,
      add: :image, type: :text
    alter_table :bot, in: :wocky_db.shared_keyspace,
      add: :type, type: :text
  end

  def down do
    alter_table :bot, in: :wocky_db.shared_keyspace, drop: :image
    alter_table :bot, in: :wocky_db.shared_keyspace, drop: :type
  end
end
