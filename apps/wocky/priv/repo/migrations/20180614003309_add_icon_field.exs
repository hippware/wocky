defmodule Wocky.Repo.Migrations.AddIconField do
  use Wocky.Repo.Migration

  def change do
    alter table(:bots) do
      add :icon, :text, null: false, default: ""
    end
  end
end
