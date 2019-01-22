defmodule Wocky.Repo.Migrations.AddLocationShares do
  use Wocky.Repo.Migration

  def change do
    create table(:user_location_shares, primary_key: false) do
      add :id, :uuid, primary_key: true
      add :user_id, references(:users, type: :uuid, on_delete: :delete_all)
      add :shared_to_id, references(:users, type: :uuid, on_delete: :delete_all)
      add :expires_at, :utc_datetime, null: false

      timestamps()
    end

    create unique_index(:user_location_shares, [:user_id, :shared_to_id])
  end
end
