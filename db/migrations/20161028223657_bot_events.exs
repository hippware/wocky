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
        created_at: :timestamp, # when the event occurred
        event:      :text       # "enter" or "exit"
      ],
      primary_key: [[:bot, :jid], :created_at, :event],
      order_by: [created_at: :desc, event: :desc]
  end

  def down do
    drop :table, named: :bot_event, in: :wocky_db.local_keyspace
  end
end
