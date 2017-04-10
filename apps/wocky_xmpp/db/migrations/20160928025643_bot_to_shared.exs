defmodule Schemata.BotToSharedMigration do
  use Schemata.Migration, [
    authored_at: "2016-09-28T02:56:43Z",
    description: "Move bot table to shared keyspace"
  ]

  def up do
    create_table :bot, in: :wocky_db.shared_keyspace,
      columns: [
        id:               :timeuuid, # Bot ID
        server:           :text,     # Bot server
        title:            :text,     # Bot title
        shortname:        :text,     # Bot shortname for URL representation
        owner:            :text,     # Bot owner
        description:      :text,     # User-supplied description
        lat:              :double,   # Latitude
        lon:              :double,   # Longditude
        radius:           :int,      # Radius of bot circle
        visibility:       :int,      # Visibility of bot
        affiliates:       {:set, :text}, # Bot's affiliates
                                         # (required for WHITELIST visibility)
        alerts:           :int       # Whether alerts are enabled (0/1)
      ],
      primary_key: :id

    create_view :user_bot, in: :wocky_db.shared_keyspace,
      from: "#{:wocky_db.shared_keyspace}.bot",
      columns: [:owner, :id],
      primary_key: [:owner, :id]

    select(:all,
      from: :bot, in: :wocky_db.local_keyspace)
    |>
    Enum.each(
     fn(v) ->
       insert into: :bot, in: :wocky_db.shared_keyspace,
       values: v
     end)

    drop :view, named: :user_bot, in: :wocky_db.local_keyspace
    drop :table, named: :bot, in: :wocky_db.local_keyspace
  end

  def down do
    create_table :bot, in: :wocky_db.local_keyspace,
      columns: [
        id:               :timeuuid, # Bot ID
        server:           :text,     # Bot server
        title:            :text,     # Bot title
        shortname:        :text,     # Bot shortname for URL representation
        owner:            :text,     # Bot owner
        description:      :text,     # User-supplied description
        lat:              :double,   # Latitude
        lon:              :double,   # Longditude
        radius:           :int,      # Radius of bot circle
        visibility:       :int,      # Visibility of bot
        affiliates:       {:set, :text}, # Bot's affiliates
                                         # (required for WHITELIST visibility)
        alerts:           :int       # Whether alerts are enabled (0/1)
      ],
      primary_key: :id

    create_view :user_bot, in: :wocky_db.local_keyspace,
      from: "#{:wocky_db.local_keyspace}.bot",
      columns: [:owner, :id],
      primary_key: [:owner, :id]

    select(:all,
      from: :bot, in: :wocky_db.shared_keyspace)
    |>
    Enum.each(
     fn(v) ->
       insert into: :bot, in: :wocky_db.local_keyspace,
       values: v
     end)

    drop :view, named: :user_bot, in: :wocky_db.shared_keyspace
    drop :table, named: :bot, in: :wocky_db.shared_keyspace
  end
end
