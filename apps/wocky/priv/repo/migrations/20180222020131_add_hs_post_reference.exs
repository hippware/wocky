defmodule Wocky.Repo.Migrations.AddHsPostReference do
  use Wocky.Repo.Migration

  def up do
    alter table(:home_stream_items) do
      add :reference_bot_item_id, :string
    end

    execute """
      ALTER TABLE home_stream_items
      ADD CONSTRAINT home_stream_items_reference_bot_item_fkey
      FOREIGN KEY (reference_bot_item_id, reference_bot_id)
      REFERENCES bot_items(id, bot_id)
    """
  end

  def down do
    alter table(:home_stream_items) do
      remove :reference_bot_item_id
    end
  end
end
