defmodule Wocky.Repo.Migrations.LocationPrimaryKey do
  use Wocky.Repo.Migration

  def up do
    drop table(:user_locations)
    create table(:user_locations, primary_key: false) do
      add :id,        :uuid, primary_key: true
      add :user_id,   references(:users, type: :uuid, on_delete: :delete_all)
      add :resource,  :string, null: false
      add :lat,       :float, null: false
      add :lon,       :float, null: false
      add :accuracy,  :float, null: false, default: 0.0

      timestamps()
    end
  end

  def down do
    drop table(:user_locations)
    create table(:user_locations, primary_key: false) do
      add :user_id,   references(:users, type: :uuid, on_delete: :delete_all), primary_key: true
      add :resource,  :string, null: false, primary_key: true
      add :lat,       :float, null: false
      add :lon,       :float, null: false
      add :accuracy,  :float, null: false

      timestamps()
    end
  end
end
