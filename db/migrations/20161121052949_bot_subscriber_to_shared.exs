defmodule Schemata.BotSubscriberToSharedMigration do
  use Schemata.Migration, [
    authored_at: "2016-11-21T05:29:49Z",
    description: "Move bot_subscriber table to shared keyspace"
  ]

  def up do
    create_table :bot_subscriber, in: :wocky_db.shared_keyspace,
      columns: [
        bot:      :timeuuid, # Bot ID
        user:     :text,     # User ID
        follow:   :boolean   # Whether user is a follower
      ],
      primary_key: [:bot, :user]

    create_view :subscribed_bot, in: :wocky_db.local_keyspace,
      from: :bot_subscriber,
      columns: :all,
      primary_key: [:user, :bot]

    select(:all,
      from: :bot_subscriber, in: :wocky_db.local_keyspace)
    |>
    Enum.each(
     fn(v) ->
       insert into: :bot_subscriber, in: :wocky_db.shared_keyspace,
       values: v
     end)

    drop :view, named: :subscribed_bot, in: :wocky_db.local_keyspace
    drop :table, named: :bot_subscriber, in: :wocky_db.local_keyspace
  end

  def down do
    create_table :bot_subscriber, in: :wocky_db.local_keyspace,
      columns: [
        bot:      :timeuuid, # Bot ID
        user:     :text,     # User ID
        follow:   :boolean   # Whether user is a follower
      ],
      primary_key: [:bot, :user]

    create_view :subscribed_bot, in: :wocky_db.local_keyspace,
      from: :bot_subscriber,
      columns: :all,
      primary_key: [:user, :bot]

    select(:all,
      from: :bot_subscriber, in: :wocky_db.shared_keyspace)
    |>
    Enum.each(
     fn(v) ->
       insert into: :bot_subscriber, in: :wocky_db.local_keyspace,
       values: v
     end)

    drop :view, named: :subscribed_bot, in: :wocky_db.shared_keyspace
    drop :table, named: :bot_subscriber, in: :wocky_db.shared_keyspace
  end
end
