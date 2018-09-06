defmodule Wocky.Repo.Migrations.AddNotificationDeleteCallback do
  use Wocky.Repo.Migration

  alias Wocky.Repo.Migration.Utils

  def up do
    Utils.update_notify("notifications", [:delete])
  end
end
