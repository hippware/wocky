defmodule Wocky.Repo.Migrations.AddShareCallback do
  use Ecto.Migration

  alias Wocky.Repo.Migration.Utils

  def up do
    Utils.add_notify("bot_shares", :insert)
  end

  def down do
    Utils.remove_notify("bot_shares", :insert)
  end
end
