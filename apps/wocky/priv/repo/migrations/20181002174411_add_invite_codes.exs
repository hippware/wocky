defmodule Wocky.Repo.Migrations.AddInviteCodes do
  use Wocky.Repo.Migration

  def change do
    create table(:user_invite_codes) do
      add :user_id, references(:users, on_delete: :delete_all, type: :binary_id)
      add :code, :string, null: false

      Wocky.Repo.Migration.timestamps(updated_at: false)
    end

    create index(:user_invite_codes, [:code])
  end
end
