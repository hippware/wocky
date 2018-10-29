defmodule Wocky.Repo.Migrations.RemoveBotShares do
  use Wocky.Repo.Migration

  def up do
    drop table(:bot_shares)

    execute "DROP FUNCTION is_visible(usr uuid, bot bots)"
  end
end
