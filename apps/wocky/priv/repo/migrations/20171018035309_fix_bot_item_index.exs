defmodule Wocky.Repo.Migrations.FixBotItemIndex do
  use Ecto.Migration
  @disable_ddl_transaction true

  def up do
    execute "ALTER TABLE bot_items DROP CONSTRAINT bot_items_pkey"
    execute "ALTER TABLE bot_items ADD PRIMARY KEY (id)"

    create index(:bot_items, [:bot_id], [concurrently: true])
  end

  def down do
    execute "ALTER TABLE bot_items DROP CONSTRAINT bot_items_pkey"
    execute "ALTER TABLE bot_items ADD PRIMARY KEY (id, bot_id)"

    drop index(:bot_items, [:bot_id], [concurrently: true])
  end
end
