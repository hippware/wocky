defmodule Wocky.Repo.Migrations.CurrentLocation do
  use Wocky.Repo.Migration

  alias Wocky.Repo.Migration.Utils

  def change do
    create table(:user_current_location, primary_key: false) do
      add :user_id, references(:users, type: :uuid, on_delete: :delete_all), primary_key: true
      add :device, :string
      add :lat, :float, null: false
      add :lon, :float, null: false
      add :accuracy, :float, null: false
      add :is_fetch, :boolean, default: false
      add :speed, :float
      add :heading, :float
      add :altitude, :float
      add :altitude_accuracy, :float
      add :captured_at, :timestamptz
      add :uuid, :string
      add :is_moving, :boolean
      add :odometer, :float
      add :activity, :string
      add :activity_confidence, :integer
      add :battery_level, :float
      add :battery_charging, :boolean

      timestamps()
    end

    Utils.update_notify(:user_current_location, [:insert, :update])
  end
end
