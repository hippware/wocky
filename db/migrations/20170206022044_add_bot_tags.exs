defmodule Schemata.AddBotTagsMigration do
  use Schemata.Migration, [
    authored_at: "2017-02-06T02:20:44Z",
    description: "Add bot tags"
  ]

  def up do
    alter_table :bot, in: :wocky_db.shared_keyspace,
      add: :tags, type: {:set, :text}
  end

  def down do
    alter_table :bot, in: :wocky_db.shared_keyspace, drop: :tags
  end
end
