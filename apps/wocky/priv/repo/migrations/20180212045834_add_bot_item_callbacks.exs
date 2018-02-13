defmodule Wocky.Repo.Migrations.AddBotItemCallbacks do
  use Wocky.Repo.Migration

  alias Wocky.Repo.Migration.Utils

  def up do
    Utils.add_notify("bot_items", [:insert, :delete])
  end

  def down do
    Utils.remove_notify("bot_items", [:insert, :delete])
  end
end
