defmodule Schemata.BotSubscriberToSharedMigration do
  use Schemata.Migration, [
    authored_at: "2016-11-21T05:29:49Z",
    description: "Move bot_subscriber table to shared keyspace"
  ]

  def up do
    create_table :bot_subscriber, in: :wocky_db.shared_keyspace,
      columns: [
        bot:      :timeuuid, # Bot ID
        server:   :text,     # Bot server
        user:     :text,     # User JID
        follow:   :boolean   # Whether user is a follower
      ],
      primary_key: [:bot, :user]

    create_view :subscribed_bot, in: :wocky_db.local_keyspace,
      from: :bot_subscriber,
      columns: :all,
      primary_key: [:user, :bot]

    # Move the data from the local to the shared table, and add a server
    # field based on the server field of the matching bot
    select(:all,
      from: :bot_subscriber, in: :wocky_db.local_keyspace)
    |>
    Enum.each(
     fn(v) ->
       result = select([:server],
         from: :bot, in: :wocky_db.shared_keyspace,
         where: %{:id => v[:bot]})
       if length(result) !== 0 do
         insert into: :bot_subscriber, in: :wocky_db.shared_keyspace,
         values: Map.put(v, :server, hd(result)[:server])
       end
     end)

    drop :view, named: :subscribed_bot, in: :wocky_db.local_keyspace
    drop :table, named: :bot_subscriber, in: :wocky_db.local_keyspace
  end

  def down do
    create_table :bot_subscriber, in: :wocky_db.local_keyspace,
      columns: [
        bot:      :timeuuid, # Bot ID
        user:     :text,     # User JID
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
       values: Map.drop(v, [:server])
     end)

    drop :view, named: :subscribed_bot, in: :wocky_db.shared_keyspace
    drop :table, named: :bot_subscriber, in: :wocky_db.shared_keyspace
  end
end
