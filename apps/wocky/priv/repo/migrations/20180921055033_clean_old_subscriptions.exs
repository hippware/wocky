defmodule Wocky.Repo.Migrations.CleanOldSubscriptions do
  use Wocky.Repo.Migration

  def up do
    execute "DELETE FROM bot_subscriptions"
    execute "DELETE FROM bot_invitations"
  end
end
