defmodule Wocky.Repo.Migrations.NillableLocation do
  use Ecto.Migration

  def change do
    drop constraint("user_bot_events", "user_bot_events_location_id_fkey")

    alter table(:user_bot_events) do
      modify :location_id, references(:user_locations, type: :uuid, on_delete: :nilify_all)
    end
  end
end
