defmodule Schemata.AddBotImageItemCountMigration do
  use Schemata.Migration, [
    authored_at: "2016-10-13T03:56:31Z",
    description: "Add bot image item count"
  ]

  def up do
    alter_table :bot_item, in: :wocky_db.local_keyspace,
      add: :image, type: :boolean
    create_view :bot_item_images,
      from: :bot_item, in: :wocky_db.local_keyspace,
      columns: [:bot, :id, :image],
      primary_key: [:bot, :image, :id]
  end

  def down do
    drop :materialized_view, named: :bot_item_images,
      in: :wocky_db.local_keyspace
    alter_table :bot_item, in: :wocky_db.local_keyspace, drop: :image
  end
end
