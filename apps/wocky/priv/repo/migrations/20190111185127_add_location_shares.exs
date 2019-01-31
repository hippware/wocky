defmodule Wocky.Repo.Migrations.AddLocationShares do
  use Wocky.Repo.Migration

  def change do
    create table(:user_location_shares) do
      add :user_id, references(:users, type: :uuid, on_delete: :delete_all)
      add :shared_with_id, references(:users, type: :uuid, on_delete: :delete_all)
      add :expires_at, :utc_datetime, null: false

      timestamps()
    end

    create unique_index(:user_location_shares, [:user_id, :shared_with_id])
  end
end