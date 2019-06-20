defmodule Wocky.Repo.Migrations.AddReadField do
  use Ecto.Migration

  def up do
    alter table(:messages) do
      add :read, :boolean, null: false, default: false
      add :updated_at, :utc_datetime
    end

    execute """
    UPDATE messages SET updated_at = created_at
    """
  end

  def down do
    alter table(:messages) do
      remove :read
      remove :updated_at
    end
  end
end
