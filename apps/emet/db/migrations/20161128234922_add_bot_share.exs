defmodule Schemata.AddBotShareMigration do
  use Schemata.Migration, [
    authored_at: "2016-11-28T23:49:22Z",
    description: "Add bot_share table"
  ]

  def up do
    create_table :bot_share, in: :wocky_db.shared_keyspace,
      columns: [
        bot:      :text,   # Full bot JID
        to_jid:   :text,   # Bare JID of user to whom the bot was shared
        from_jid: :text,   # Bare JID of user who shared the bot
        time:   :timestamp # Time at which the bot was shared
      ],
      primary_key: [:bot, :to_jid]
  end

  def down do
    drop :table, named: :bot_share, in: :wocky_db.shared_keyspace
  end
end
