defmodule Wocky.Repo.Migrations.AddTrafficTable do
  use Wocky.Repo.Migration

  def change do
    create table(:traffic_logs) do
      add :user_id,    references(:users, type: :uuid, on_delete: :nothing)
      add :resource,   :string, null: false
      add :ip,         :string, null: false
      add :incoming,   :boolean, null: false
      add :packet,     :binary, null: false

      Wocky.Repo.Migration.timestamps(updated_at: false)
    end
  end
end
