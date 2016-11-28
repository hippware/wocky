defmodule Schemata.AddTempSubscriptionsMigration do
  use Schemata.Migration, [
    authored_at: "2016-11-28T00:00:14Z",
    description: "Add temporary bot subscriptions"
  ]

  def up do
    drop :materialized_view, named: "#{:wocky_db.shared_keyspace}.subscribed_bot"

    alter_table "#{:wocky_db.shared_keyspace}.bot_subscriber",
      drop: :follow

    create_subscribed_bot

    create_table :temp_subscription, in: :wocky_db.shared_keyspace,
      columns: [
        device:    :text,     # User device full JID
        bot:       :timeuuid, # Bot ID
        server:    :text,     # Bot server
        node:      :text      # Erlang node to which device is connected
      ],
      primary_key: [:device, :bot]

    create_view "#{:wocky_db.shared_keyspace}.bot_temp_subscription",
      from: :temp_subscription,
      columns: :all,
      primary_key: [:bot, :device]


    create_view "#{:wocky_db.shared_keyspace}.node_temp_subscription",
      from: :temp_subscription,
      columns: [:device, :bot, :node],
      primary_key: [:node, :device, :bot]
  end

  def down do
    drop :materialized_view, named: "#{:wocky_db.shared_keyspace}.node_temp_subscription"
    drop :materialized_view, named: "#{:wocky_db.shared_keyspace}.bot_temp_subscription"
    drop :table, named: :temp_subscription, in: :wocky_db.shared_keyspace

    drop :materialized_view, named: "#{:wocky_db.shared_keyspace}.subscribed_bot"

    alter_table :bot_subscriber, in: :wocky_db.shared_keyspace,
      add: :follow, type: :boolean

    create_view :subscribed_bot,
      from: :bot_subscriber, in: :wocky_db.shared_keyspace,
      columns: :all,
      primary_key: [:user, :bot]

    create_subscribed_bot
  end

  defp create_subscribed_bot do
    create_view "#{:wocky_db.shared_keyspace}.subscribed_bot",
      from: :bot_subscriber,
      columns: :all,
      primary_key: [:user, :bot]
  end


end
