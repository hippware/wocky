defmodule Schemata.AddBotTablesMigration do
  use Schemata.Migration, [
    authored_at: "2016-08-19T02:13:19Z",
    description: "Add tables for bots"
  ]

  def up do
    create_table :bot, in: :wocky_db.local_keyspace,
      columns: [
        id:           :timeuuid,
        server:       :text,
        title:        :text,
        shortname:    :text,
        owner:        :text,
        description:  :text,
        lat:          :double,
        lon:          :double,
        radius:       :int,
        visibility:   :int,
        affiliates:   {:set, :text},
        followers:    {:set, :text},
        alerts:       :int
      ],
      primary_key: :id

    create_table :bot_name, in: :wocky_db.local_keyspace,
      columns: [
        shortname:    :text,
        id:           :timeuuid
      ],
      primary_key: :shortname

    create_view :user_bot, in: :wocky_db.local_keyspace,
      from: :bot,
      columns: [:owner, :id],
      primary_key: [:owner, :id]
  end

  def down do
    drop :materialized_view, named: :user_bot, in: :wocky_db.local_keyspace
    drop :table, named: :bot_name, in: :wocky_db.local_keyspace
    drop :table, named: :bot, in: :wocky_db.local_keyspace
  end
end
