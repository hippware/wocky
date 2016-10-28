defmodule Schemata.BotEventsMigration do
  use Schemata.Migration, [
    authored_at: "2016-10-28T22:36:57Z",
    description: "Create bot_event table"
  ]

  def up do
    create_table :bot_event, in: :wocky_db.local_keyspace,
      columns: [
        bot:        :timeuuid,  # Bot ID
        jid:        :text,      # User JID
        event:      :text,      # "enter" or "exit"
        created_at: :timestamp  # when the event occurred
      ],
      primary_key: [[:bot, :jid], :event, :created_at],
      order_by: [event: :asc, created_at: :desc]
  end

  def down do
    drop :table, named: :bot_event, in: :wocky_db.local_keyspace
  end
end
