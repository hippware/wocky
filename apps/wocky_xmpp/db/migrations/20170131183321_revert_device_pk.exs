defmodule Schemata.RevertDevicePkMigration do
  use Schemata.Migration, [
    authored_at: "2017-01-31T18:33:21Z",
    description: "Reverts the changes in 20161206184714_change-device-pk"
  ]

  def up do
    drop :view, named: "#{:wocky_db.local_keyspace}.device_resource"
    drop :table, named: :device, in: :wocky_db.local_keyspace
    create_table :device, in: :wocky_db.local_keyspace,
       columns: [
         user:       :text,
         server:     :text,
         resource:   :text,
         platform:   :text,
         device_id:  :text,
         endpoint:   :text,
         created_at: :timestamp
       ],
       primary_key: [:user, :server, :resource]
  end

  def down do
    drop :table, named: :device, in: :wocky_db.local_keyspace
    create_table :device, in: :wocky_db.local_keyspace,
       columns: [
         user:       :text,
         server:     :text,
         device_id:  :text,
         resource:   :text,
         platform:   :text,
         endpoint:   :text,
         created_at: :timestamp
       ],
       primary_key: [:user, :server, :device_id]

    create_view :device_resource,
      from: :device, in: :wocky_db.local_keyspace,
      columns: :all,
      primary_key: [:user, :server, :resource, :device_id]
  end
end
