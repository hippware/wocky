defmodule Wocky.Repo.Migrations.AddRosterCallbacks do
  use Wocky.Repo.Migration

  alias Wocky.Repo.Migration.Utils

  def up,
    do: Utils.update_notify("roster_items", [:insert, :update])

  def down,
    do: Utils.remove_notify("roster_items", [:insert, :update])
end
