defmodule Wocky.Repo.Migrations.AddLocationsTable do
  use Ecto.Migration

  def change do
    create table(:locations, primary_key: false) do
      add :user_id,   references(:users, type: :uuid, on_delete: :delete_all), primary_key: true
      add :resource,  :string, null: false, primary_key: true
      add :lat,       :double, null: false
      add :lon,       :double, null: false
      add :accuracy,  :double, null: false

      timestamps()
    end
  end
end
