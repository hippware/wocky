defmodule Schemata.RosterQueryPermissionsMigration do
  use Schemata.Migration, [
    authored_at: "2016-09-02T00:46:14Z",
    description: "Add bot subscribers and owner roster"
  ]

  def up do
    alter_table :user, in: :wocky_db.shared_keyspace,
      add: :roster_viewers, type: {:set, :text}

    alter_table :bot, in: :wocky_db.local_keyspace,
      drop: :followers
    alter_table :bot, in: :wocky_db.local_keyspace,
      add: :owner_roster, type: {:set, :text}
    alter_table :bot, in: :wocky_db.local_keyspace,
      add: :owner_roster_ver, type: :text

    create_table :bot_subscriber, in: :wocky_db.local_keyspace,
      columns: [
        bot:    :timeuuid,
        user:   :text,
        follow: :boolean
      ],
      primary_key: [:bot, :user]
  end

  def down do
    drop :materialized_view, named: :external_id_to_user, in: :wocky_db.shared_keyspace
    alter_table :user, in: :wocky_db.shared_keyspace,
      drop: :roster_viewers
    create_view :external_id_to_user,
      from: :user, in: :wocky_db.shared_keyspace,
      columns: :all,
      primary_key: [:external_id, :server, :user]

    alter_table :bot, in: :wocky_db.local_keyspace,
      add: :followers, type: {:set, :text}
    alter_table :bot, in: :wocky_db.local_keyspace,
      drop: :owner_roster
    alter_table :bot, in: :wocky_db.local_keyspace,
      drop: :owner_roster_ver

    drop :table, named: :bot_subscriber, in: :wocky_db.local_keyspace
  end
end
