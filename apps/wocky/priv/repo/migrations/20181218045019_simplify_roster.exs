defmodule Wocky.Repo.Migrations.SimplifyRoster do
  use Wocky.Repo.Migration

  def up do
    execute """
    DELETE FROM roster_items WHERE subscription != 'both'
    """

    alter table(:roster_items) do
      remove :ask
      remove :subscription
      remove :groups
    end
  end

  def down do
    alter table(:roster_items) do
      add :ask, :string, null: false
      add :subscription, :string, null: false
      add :groups, [:string], null: false
    end
  end
end
