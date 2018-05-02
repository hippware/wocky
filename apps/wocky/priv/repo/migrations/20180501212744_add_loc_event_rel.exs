defmodule Wocky.Repo.Migrations.AddLocEventRel do
  use Ecto.Migration

  def change do
    alter table(:user_bot_events) do
      add :location_id, references(:user_locations, type: :uuid)
    end
  end
end
