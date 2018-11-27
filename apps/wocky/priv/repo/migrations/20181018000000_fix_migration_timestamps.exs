defmodule Wocky.Repo.Migrations.FixMigrationTimestamps do
  use Ecto.Migration

  def change do
    alter table(:schema_migrations) do
      modify :inserted_at, :naive_datetime
    end
  end
end
