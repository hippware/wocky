defmodule Wocky.Repo.Migrations.AddShareUpdateTrigger do
  use Wocky.Repo.Migration

  alias Wocky.Repo.Migration.Utils

  def up do
    Utils.update_notify("user_location_shares", [:update])
  end

  def down do
    Utils.remove_notify("user_location_shares", :update)
  end
end
