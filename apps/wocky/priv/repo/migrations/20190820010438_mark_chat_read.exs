defmodule Wocky.Repo.Migrations.MarkChatRead do
  use Wocky.Repo.Migration

  def up do
    execute "UPDATE messages SET read=true"

    execute "ALTER TABLE messages DROP migration_id"
  end

  def down do
    :ok
  end
end
