defmodule Wocky.Repo.Migrations.AddShareGeofenceField do
  use Wocky.Repo.Migration

  def change do
    alter table(:bot_shares) do
      add :geofence, :boolean, default: false
    end
  end
end
