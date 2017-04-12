defmodule Wocky.Repo.Migrations.AddBotEventsTable do
  use Ecto.Migration

  def change do
    create table(:bot_events, primary_key: false) do
      add :id,        :uuid, primary_key: true
      add :user_id,   references(:users, type: :uuid, on_delete: :delete_all), null: false
      add :bot_id,    references(:bots, type: :uuid, on_delete: :delete_all), null: false
      add :event,     :string, null: false

      timestamps()
    end
  end
end
