defmodule Schemata.AddFileMetadataMigration do
  use Schemata.Migration, [
    authored_at: "2017-02-13T05:41:21Z",
    description: "<your-description-here>"
  ]

  def up do
    create_table :file_metadata, in: :wocky_db.shared_keyspace,
      columns: [
        id:       :text,
        server:   :text,
        access:   :text,
        owner:    :text
      ],
      primary_key: [:id, :server]
  end

  def down do
    drop :table, named: :file_metadata, in: :wocky_db.shared_keyspace
  end
end
