defmodule Wocky.Repo.Migrations.AddDelSubscriberCallback do
  use Wocky.Repo.Migration

  alias Wocky.Repo.Migration.Utils

  def up do
    Utils.update_notify("bot_subscriptions", [:delete])
  end

  def down do
    Utils.remove_notify("bot_subscriptions", :delete)
  end
end
