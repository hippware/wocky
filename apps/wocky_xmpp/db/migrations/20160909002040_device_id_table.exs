defmodule Schemata.DeviceIdTableMigration do
  use Schemata.Migration, [
    authored_at: "2016-09-09T00:20:40Z",
    description: "Create the device table"
  ]

  def up do
    create_table :device, in: :wocky_db.local_keyspace,
      columns: [
        user:       :text,      # User ID (userpart of JID)
        server:     :text,      # Server (domainpart of JID)
        resource:   :text,      # Resource (resourcepart of JID)
        platform:   :text,      # Client OS platform (apple or google)
        device_id:  :text,      # Device ID
        endpoint:   :text,      # SNS application endpoint ARN
        created_at: :timestamp  # When the device was registered
      ],
      primary_key: [:user, :server, :resource]
  end

  def down do
    drop :table, named: :device, in: :wocky_db.local_keyspace
  end
end
