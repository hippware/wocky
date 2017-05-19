defmodule Wocky.Repo.Migrations.FixBotDescSize do
  use Ecto.Migration

  def change do
    alter table(:bots) do
      remove :description
      add :description, :binary
    end
  end
end
