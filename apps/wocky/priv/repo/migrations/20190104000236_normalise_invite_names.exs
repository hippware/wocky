defmodule Wocky.Repo.Migrations.NormaliseInviteNames do
  use Wocky.Repo.Migration

  def change do
    rename table(:notifications), :invitation_id, to: :bot_invitation_id
    rename table(:notifications), :invitation_accepted, to: :bot_invitation_accepted
  end
end
