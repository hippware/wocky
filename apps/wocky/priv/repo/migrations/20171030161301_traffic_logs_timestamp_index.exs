defmodule Wocky.Repo.Migrations.TrafficLogsTimestampIndex do
  use Wocky.Repo.Migration

  def change do
    create index(:traffic_logs, [:created_at])
    create index(:notification_logs, [:created_at])
  end
end
