defmodule Wocky.Repo.Migrations.FixBotItemIndex do
  use Ecto.Migration
  @disable_ddl_transaction true

  def change do
    create index(:bot_items, [:bot_id], [concurrently: true])
  end
end
