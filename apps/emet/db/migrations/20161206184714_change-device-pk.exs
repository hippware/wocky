defmodule Schemata.ChangeDevicePkMigration do
  use Schemata.Migration, [
    authored_at: "2016-12-06T18:47:14Z",
    description: "Change the device table primary key"
  ]

  def up do
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

  def down do
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
end
