defmodule Wocky.Repo.Migrations.AddUserGeolocTables do
  use Wocky.Repo.Migration

  def change do
    create table(:user_locations, primary_key: false) do
      add :user_id,   references(:users, type: :uuid, on_delete: :delete_all), primary_key: true
      add :resource,  :string, null: false, primary_key: true
      add :lat,       :double, null: false
      add :lon,       :double, null: false
      add :accuracy,  :double, null: false

      timestamps()
    end

    create table(:user_bot_events, primary_key: false) do
      add :id,        :uuid, primary_key: true
      add :user_id,   references(:users, type: :uuid, on_delete: :delete_all), null: false
      add :bot_id,    references(:bots, type: :uuid, on_delete: :delete_all), null: false
      add :event,     :string, null: false

      timestamps()
    end
  end
end
