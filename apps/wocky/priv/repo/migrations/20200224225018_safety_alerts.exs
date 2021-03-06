defmodule Wocky.Repo.Migrations.SafetyAlerts do
  use Wocky.Repo.Migration

  def change do
    create table("safety_alerts", primary_key: false) do
      add :id, :uuid, null: false, primary_key: true
      add :source, :text, null: false
      add :source_id, :text, null: false
      add :created_at, :timestamptz, null: false
      add :updated_at, :timestamptz, null: false
      add :expires_at, :timestamptz
      add :title, :text, null: false
      add :summary, :text, null: false
      add :link, :text
      add :geometry, :geometry, null: false
      add :data, :map, null: false, default: %{}
      add :imported, :boolean, null: false, default: true
    end

    create table("safety_alerts_geometries") do
      add :source, :text, null: false
      add :source_id, :text, null: false
      add :geometry, :geometry, null: false
      add :data, :map, null: false, default: %{}
      add :created_at, :timestamptz, null: false
      add :updated_at, :timestamptz, null: false
    end

    create index("safety_alerts", [:source, :source_id], unique: true)
    create index("safety_alerts_geometries", [:source, :source_id], unique: true)
  end
end
