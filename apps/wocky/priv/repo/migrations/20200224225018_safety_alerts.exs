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
    end

    create index("safety_alerts", [:source, :source_id], unique: true)
  end
end
