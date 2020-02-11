defmodule Wocky.Repo.Migrations.AddNearbyColumn do
  use Wocky.Repo.Migration

  def change do
    alter table(:roster_items) do
      add :nearby, :boolean, null: false, default: false
    end
  end
end
