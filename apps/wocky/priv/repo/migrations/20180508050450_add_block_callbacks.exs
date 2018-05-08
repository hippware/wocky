defmodule Wocky.Repo.Migrations.AddBlockCallbacks do
  use Wocky.Repo.Migration

  alias Wocky.Repo.Migration.Utils

  def change do
    Utils.update_notify("blocks", :insert)
  end

  def down do
    Utils.remove_notify("blocks", :insert)
  end
end
