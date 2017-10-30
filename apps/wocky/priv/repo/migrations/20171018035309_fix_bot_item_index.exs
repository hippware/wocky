defmodule Wocky.Repo.Migrations.FixBotItemIndex do
  use Wocky.Repo.Migration
  @disable_ddl_transaction true

  def change do
    create index(:bot_items, [:bot_id], [concurrently: true])
  end
end
