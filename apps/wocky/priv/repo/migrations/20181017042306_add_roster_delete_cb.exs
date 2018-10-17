defmodule Wocky.Repo.Migrations.AddRosterDeleteCb do
  use Wocky.Repo.Migration

  alias Wocky.Repo.Migration.Utils

  def up,
    do: Utils.update_notify("roster_items", [:delete])

  def down,
    do: Utils.remove_notify("roster_items", [:delete])
end
