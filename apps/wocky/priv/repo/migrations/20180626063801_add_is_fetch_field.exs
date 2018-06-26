defmodule Wocky.Repo.Migrations.AddIsFetchField do
  use Wocky.Repo.Migration

  def change do
    alter table(:user_locations) do
      add :is_fetch, :boolean, default: false
    end
  end
end
