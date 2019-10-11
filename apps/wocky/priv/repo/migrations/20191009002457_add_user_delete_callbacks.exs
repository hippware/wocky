defmodule Wocky.Repo.Migrations.AddUserDeleteCallbacks do
  use Wocky.Repo.Migration

  import DawdleDB.Migration

  def up do
    update_notify("users", [:delete])
  end

  def down do
    remove_notify("users", [:delete])
  end
end
