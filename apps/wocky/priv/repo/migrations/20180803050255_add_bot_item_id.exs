defmodule Wocky.Repo.Migrations.AddBotItemId do
  @moduledoc false

  use Wocky.Repo.Migration

  import Ecto.Query

  alias Wocky.ID
  alias Wocky.Repo

  def change do
    execute """
      ALTER TABLE home_stream_items
      DROP CONSTRAINT home_stream_items_reference_bot_item_fkey
    """

    rename table(:bot_items), :id, to: :tmp_id
    rename table(:home_stream_items), :reference_bot_item_id, to: :old_ref_item_id

    alter table(:bot_items) do
      add :id, :uuid
    end

    flush()

    from(i in "bot_items")
    |> select([:tmp_id, :bot_id])
    |> Repo.stream()
    |> Stream.each(&add_id/1)
    |> Stream.run()

    alter table(:bot_items) do
      remove :tmp_id
      modify :id, :uuid, primary_key: true
    end

    alter table(:home_stream_items) do
      add :reference_bot_item_id, :uuid
      remove :old_ref_item_id
    end

    execute """
      ALTER TABLE home_stream_items
      ADD CONSTRAINT home_stream_items_reference_bot_item_fkey
      FOREIGN KEY (reference_bot_item_id)
      REFERENCES bot_items(id)
    """
  end

  defp add_id(%{tmp_id: tmp_id, bot_id: bot_id}) do
    id = ID.new()

    execute """
    UPDATE bot_items SET id = #{id}
    WHERE tmp_id = #{tmp_id} AND bot_id = #{bot_id}
    """

    execute """
    UPDATE home_stream_items SET reference_bot_item_id = #{id}
    WHERE old_ref_item_id = #{tmp_id} AND reference_bot_id = #{bot_id}
    """
  end
end
