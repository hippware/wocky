defmodule Schemata.DefaultPrivacyListMigration do
  use Schemata.Migration, [
    authored_at: "2016-08-19T01:23:34Z",
    description: "Remove fields for default privacy list and ask message"
  ]

  def up do
    alter_table :roster, in: :wocky_localhost, drop: :ask_message
    alter_table :privacy, in: :wocky_localhost, drop: :default
  end

  def down do
    alter_table :roster, in: :wocky_localhost, add: :ask_message, type: :text
    alter_table :privacy, in: :wocky_localhost, add: :default, type: :text
  end
end
