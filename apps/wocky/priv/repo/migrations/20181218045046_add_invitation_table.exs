defmodule Wocky.Repo.Migrations.AddInvitationTable do
  use Wocky.Repo.Migration

  alias Wocky.Repo.Migration.Utils

  def up do
    create table(:user_invitations) do
      add :user_id, references(:users, type: :uuid, on_delete: :delete_all)
      add :invitee_id, references(:users, type: :uuid, on_delete: :delete_all)

      timestamps()
    end

    create unique_index(:user_invitations, [:user_id, :invitee_id])

    Utils.update_notify(:user_invitations, :insert)
  end
end
