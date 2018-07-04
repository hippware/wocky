defmodule Wocky.Repo.Migrations.AddLocFields do
  use Ecto.Migration

  def change do
    alter table(:user_locations) do
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
    end
  end
end
