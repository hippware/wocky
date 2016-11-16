defmodule Schemata.BotFollowMeMigration do
  use Schemata.Migration, [
    authored_at: "2016-11-08T22:09:57Z",
    description: "Add fields for follow me to bot"
  ]

  def up do
    drop :view, named: :user_bot, in: :wocky_db.shared_keyspace

    alter_table :bot, in: :wocky_db.shared_keyspace,
      add: :follow_me, type: :boolean

    alter_table :bot, in: :wocky_db.shared_keyspace,
      add: :follow_me_expiry, type: :timestamp

    create_view :user_bot, in: :wocky_db.shared_keyspace,
      from: :bot,
      columns: [:owner, :id, :follow_me, :follow_me_expiry],
      primary_key: [:owner, :id]
  end

  def down do
    drop :view, named: :user_bot, in: :wocky_db.shared_keyspace

    alter_table :bot, in: :wocky_db.shared_keyspace, drop: :follow_me
    alter_table :bot, in: :wocky_db.shared_keyspace, drop: :follow_me_expiry

    create_view :user_bot, in: :wocky_db.shared_keyspace,
      from: :bot,
      columns: [:owner, :id],
      primary_key: [:owner, :id]
  end
end
