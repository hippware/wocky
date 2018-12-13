defmodule Wocky.Repo.Migrations.RemoveXmppTables do
  use Ecto.Migration

  def up do
    drop table("last")
    drop table("offline_message")
  end
end
