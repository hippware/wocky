defmodule Wocky.Repo.Migrations.AddSubscribersCallbacks do
  use Wocky.Repo.Migration

  alias Wocky.Repo.Migration.Utils

  def up do
    Utils.update_notify("bot_subscriptions", [:insert, :update])
  end

  def down do
    Utils.remove_notify("bot_subscriptions", :insert)
    Utils.remove_notify("bot_subscriptions", :update)
  end
end
