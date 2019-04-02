defmodule Wocky.Repo.Migrations.FixBotItemIndex do
  use Wocky.Repo.Migration

  def change do
    create index(:bot_items, [:bot_id])
  end
end
