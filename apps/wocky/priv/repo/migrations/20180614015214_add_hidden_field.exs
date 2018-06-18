defmodule Wocky.Repo.Migrations.AddHiddenField do
  use Wocky.Repo.Migration

  def change do
    alter table(:users) do
      add :hidden_until, :timestamptz
    end
  end
end
