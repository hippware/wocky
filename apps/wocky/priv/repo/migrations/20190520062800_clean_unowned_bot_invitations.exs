defmodule Wocky.Repo.Migrations.CleanUnownedBotInvitations do
  use Wocky.Repo.Migration

  def up do
    execute """
    DELETE FROM bot_invitations i USING bots b
    WHERE i.bot_id = b.id AND i.user_id != b.user_id
    """
  end
end
