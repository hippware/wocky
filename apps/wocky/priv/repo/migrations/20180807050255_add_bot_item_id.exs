defmodule Wocky.Repo.Migrations.AddBotItemId do
  @moduledoc false

  use Wocky.Repo.Migration

  def change do
    execute "CREATE EXTENSION IF NOT EXISTS \"uuid-ossp\""

    execute """
      ALTER TABLE home_stream_items
      DROP CONSTRAINT home_stream_items_reference_bot_item_fkey
    """

    rename table(:bot_items), :id, to: :tmp_id
    rename table(:home_stream_items), :reference_bot_item_id, to: :old_ref_item_id

    alter table(:bot_items) do
      add :id, :uuid
    end

    alter table(:home_stream_items) do
      add :reference_bot_item_id, :uuid
    end

    flush()

    execute "UPDATE bot_items SET id = uuid_generate_v1()"

    execute """
    UPDATE home_stream_items
    SET reference_bot_item_id = bot_items.id
    FROM bot_items
    WHERE home_stream_items.reference_bot_id = bot_items.bot_id
    AND home_stream_items.old_ref_item_id = bot_items.tmp_id
    """

    alter table(:bot_items) do
      remove :tmp_id
      modify :id, :uuid, primary_key: true
    end

    alter table(:home_stream_items) do
      remove :old_ref_item_id
    end

    flush()

    execute """
    ALTER TABLE home_stream_items
    ADD CONSTRAINT home_stream_items_reference_bot_item_fkey
    FOREIGN KEY (reference_bot_item_id)
    REFERENCES bot_items(id)
    """
  end
end
