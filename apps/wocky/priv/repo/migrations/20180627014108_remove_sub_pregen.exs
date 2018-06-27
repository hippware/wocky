defmodule Wocky.Repo.Migrations.RemoveSubPregen do
  use Ecto.Migration

  @events ["insert", "update", "delete", "truncate"]

  def up do
    Enum.each(@events, &drop_fun_and_trigger(&1))

    execute "DROP FUNCTION IF EXISTS update_bot_subscribers(uuid)"

    alter table(:bots) do
      remove :subscribers_count
      remove :subscribers_hash
      remove :guests_count
      remove :guests_hash
      remove :visitors_count
      remove :visitors_hash
    end
  end

  defp drop_fun_and_trigger(event) do
    execute "DROP TRIGGER IF EXISTS #{event}_bot_subscribers_trigger ON bot_subscriptions"
    execute "DROP TRIGGER IF EXISTS #{event}_bot_subscribers_before_trigger ON bot_subscriptions"
    execute "DROP FUNCTION IF EXISTS #{event}_bot_subscribers_trigger()"
    execute "DROP FUNCTION IF EXISTS #{event}_bot_subscribers_before_trigger()"
  end

end
