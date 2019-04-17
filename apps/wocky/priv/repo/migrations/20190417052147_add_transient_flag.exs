defmodule Wocky.Repo.Migrations.AddTransientFlag do
  use Ecto.Migration

  def change do
    alter table(:users) do
      add :transient, :boolean, default: false, null: false
    end
  end
end
