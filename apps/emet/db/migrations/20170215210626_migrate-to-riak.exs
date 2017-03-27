defmodule Schemata.MigrateToRiakMigration do
  use Schemata.Migration, [
    authored_at: "2017-02-15T21:06:26Z",
    description: "DO NOT RUN IN PRODUCTION. Makes the migrations check happy."
  ]

  def up do
    drop :table, named: :group_chat, in: :wocky_db.local_keyspace
    drop :table, named: :last_activity, in: :wocky_db.local_keyspace
    drop :table, named: :offline_msg, in: :wocky_db.local_keyspace
  end
end
