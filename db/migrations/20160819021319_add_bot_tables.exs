defmodule Schemata.AddBotTablesMigration do
  use Schemata.Migration, [
    authored_at: "2016-08-19T02:13:19Z",
    description: "Add tables for bots"
  ]

  def up do
    create_table :bot, in: :wocky_localhost,
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

    create_table :bot_name, in: :wocky_localhost,
      columns: [
        shortname:    :text,
        id:           :timeuuid
      ],
      primary_key: :shortname

    create_view :user_bot, in: :wocky_localhost,
      from: :bot,
      columns: [:owner, :id],
      primary_key: [:owner, :id]
  end

  def down do
    drop :materialized_view, named: :user_bot, in: :wocky_localhost
    drop :table, named: :bot_name, in: :wocky_localhost
    drop :table, named: :bot, in: :wocky_localhost
  end
end
