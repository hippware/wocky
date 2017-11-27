defmodule Wocky.Repo.Migrations.CreatePushTokens do
  use Wocky.Repo.Migration

  def change do
    create table(:push_tokens, primary_key: false) do
      add :id, :binary_id, primary_key: true
      add :user_id, references(:users, on_delete: :delete_all, type: :binary_id)
      add :resource, :string, null: false
      add :token, :string, null: false
      add :valid, :boolean, null: false, default: true
      add :enabled_at, :timestamptz
      add :disabled_at, :timestamptz
      add :invalidated_at, :timestamptz

      Wocky.Repo.Migration.timestamps(updated_at: false)
    end

    create index(:push_tokens, [:user_id])
    create unique_index(:push_tokens, [:user_id, :resource, :token])
  end
end
