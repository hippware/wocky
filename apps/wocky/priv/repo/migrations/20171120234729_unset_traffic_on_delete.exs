defmodule Wocky.Repo.Migrations.UnsetTrafficOnDelete do
  use Wocky.Repo.Migration

  def up do
    execute "ALTER TABLE traffic_logs DROP CONSTRAINT traffic_logs_user_id_fkey"
    create index(:traffic_logs, [:user_id])
  end

  def down do
    drop index(:traffic_logs, [:user_id])
    alter table(:traffic_logs) do
      modify :user_id, references(:users, type: :uuid, on_delete: :delete_all)
    end
  end
end
