defmodule Schemata.BotFollowMeMigration do
  use Schemata.Migration, [
    authored_at: "2016-11-08T22:09:57Z",
    description: "Add fields for follow me to bot"
  ]

  def up do
    alter_table :bot, in: :wocky_db.shared_keyspace,
      add: :follow_me, type: :boolean

    alter_table :bot, in: :wocky_db.shared_keyspace,
      add: :follow_me_expiry, type: :timestamp
  end

  def down do
    alter_table :bot, in: :wocky_db.shared_keyspace, drop: :follow_me
    alter_table :bot, in: :wocky_db.shared_keyspace, drop: :follow_me_expiry
  end
end
