defmodule Wocky.Repo.Migrations.MigrateShares do
  use Wocky.Repo.Migration

  def up do
    execute """
    INSERT INTO bot_invitations (user_id, invitee_id, bot_id, accepted, created_at, updated_at)
      SELECT sharer_id, user_id, bot_id, true, created_at, updated_at
      FROM bot_shares
      WHERE sharer_id IS NOT NULL
    ON CONFLICT DO NOTHING
    """
  end
end
