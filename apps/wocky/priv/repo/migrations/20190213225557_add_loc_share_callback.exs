defmodule Wocky.Repo.Migrations.AddLocShareCallback do
  use Wocky.Repo.Migration

  alias Wocky.Repo.Migration.Utils

  def up,
    do: Utils.update_notify("user_location_shares", [:insert, :delete])

  def down,
    do: Utils.remove_notify("user_location_shares", [:insert, :delete])
end
