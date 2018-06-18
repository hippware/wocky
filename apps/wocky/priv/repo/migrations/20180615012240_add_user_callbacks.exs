defmodule Wocky.Repo.Migrations.AddUserCallbacks do
  use Wocky.Repo.Migration

  alias Wocky.Repo.Migration.Utils

  def up do
    Utils.update_notify("users", :update)
  end

  def down do
    Utils.remove_notify("users", :update)
  end
end
