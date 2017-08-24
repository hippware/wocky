defmodule Wocky.Repo.Migrations.AddItemAuthor do
  use Wocky.Repo.Migration
  use Wocky.Repo.Model

  alias Wocky.Bot.Item

  def up do
    alter table(:bot_items) do
      add :user_id, references(:users, type: :uuid, on_delete: :delete_all, null: false)
    end

    flush()

    Repo.transaction fn ->
      Item
      |> preload(:bot)
      |> Repo.stream
      |> Stream.map(&set_user(&1))
      |> Enum.to_list
    end
  end

  def down do
    alter table(:bot_items) do
      remove :user_id
    end
  end

  defp set_user(%Item{id: id, bot: bot}) do
    Item
    |> where([i], i.id == ^id)
    |> Repo.update_all(set: [user_id: bot.user_id])
  end
end
