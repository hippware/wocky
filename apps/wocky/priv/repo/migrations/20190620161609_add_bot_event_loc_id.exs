defmodule Wocky.Repo.Migrations.AddBotEventLocId do
  use Ecto.Migration

  def change do
    alter table(:user_bot_events) do
      add :location_id, :string
    end
  end
end
