defmodule Wocky.Repo.Migrations.AddHsItemClassField do
  use Wocky.Repo.Migration

  def up do
    alter table(:home_stream_items) do
      add :class, :string, size: 10, null: false, default: "item"
    end

    flush()

    execute """
    UPDATE home_stream_items SET class = 'deleted' WHERE deleted
    """

    alter table(:home_stream_items) do
      remove :deleted
    end
  end

  def down do
    alter table(:home_stream_items) do
      add :deleted, :boolean, null: false, default: false
    end

    flush()

    execute """
    UPDATE home_stream_items SET deleted = true WHERE class = 'deleted'
    """

    alter table(:home_stream_items) do
      remove :class
    end
  end
end
