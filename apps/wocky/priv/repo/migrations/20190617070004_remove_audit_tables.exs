defmodule Wocky.Repo.Migrations.RemoveAuditTables do
  use Ecto.Migration

  def up do
    alter table(:user_bot_events) do
      remove :location_id
    end

    drop table(:traffic_logs)
    drop table(:user_locations)
    drop table(:push_logs)
  end
end
