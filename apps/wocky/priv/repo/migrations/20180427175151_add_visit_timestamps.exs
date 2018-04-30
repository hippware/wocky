defmodule Wocky.Repo.Migrations.AddVisitTimestamps do
  use Wocky.Repo.Migration

  def change do
    alter table(:bot_subscriptions) do
      add :visited_at, :timestamptz
      add :departed_at, :timestamptz
    end
  end
end
