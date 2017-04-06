defmodule Schemata.RemoveWockySmMigration do
  use Schemata.Migration, [
    authored_at: "2016-11-21T00:01:25Z",
    description: "Remove wocky session manager table"
  ]

  def up do
    drop :materialized_view, named: :user_sessions, in: :wocky_db.local_keyspace
    drop :table, named: :session, in: :wocky_db.local_keyspace
  end

  def down do
    create_table :session, in: :wocky_db.local_keyspace,
      columns: [
        sid:          :blob,
        node:         :text,
        user:         :text,
        server:       :text,
        jid_user:     :text,
        jid_server:   :text,
        jid_resource: :blob,
        priority:     :int,
        info:         :blob
      ],
      primary_key: [:sid, :jid_user]

    create_index in: :wocky_db.local_keyspace, on: :session, keys: [:node]

    create_view :user_sessions, in: :wocky_db.local_keyspace,
      from: :session,
      columns: :all,
      primary_key: [:jid_user, :jid_resource, :sid]
  end
end
