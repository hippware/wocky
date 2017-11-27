defmodule Wocky.Repo.Migrations.CreatePushLogs do
  use Wocky.Repo.Migration

  def change do
    create table(:push_logs, primary_key: false) do
      add :id, :binary_id, primary_key: true
      add :user_id, references(:users, on_delete: :delete_all, type: :binary_id)
      add :resource, :string, null: false
      add :token, :string, null: false
      add :message_id, :string, null: false
      add :payload, :text, null: false
      add :response, :string, null: false
      add :details, :text

      Wocky.Repo.Migration.timestamps(updated_at: false)
    end

    create index(:push_logs, [:user_id])
  end
end
