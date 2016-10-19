defmodule Schemata.AddSubscribedBotViewMigration do
  use Schemata.Migration, [
    authored_at: "2016-10-18T00:33:12Z",
    description: "Add subscribed bot view"
  ]

  def up do
    alter_table :bot, in: :wocky_db.shared_keyspace,
      add: :updated, type: :timestamp
    create_view :subscribed_bot,
      from: :bot_subscriber, in: :wocky_db.local_keyspace,
      columns: :all,
      primary_key: [:user, :bot]
  end

  def down do
    alter_table :bot, in: :wocky_db.shared_keyspace, drop: :updated
    drop :materialized_view, named: :subscribed_bot, in: :wocky_db.local_keyspace
  end
end
