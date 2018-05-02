defmodule Wocky.Repo.Migrations.AddVisitTimestamps do
  use Wocky.Repo.Migration

  def change do
    alter table(:bot_subscriptions) do
      add :visited_at, :timestamptz
      add :departed_at, :timestamptz
    end

    flush()

    execute """
    CREATE VIEW bot_activity AS
      SELECT bot_id, MAX(visited_at) AS visited_at
        FROM bot_subscriptions AS subs
        WHERE EXISTS (
          SELECT 1
          FROM bot_subscriptions
          WHERE bot_id = subs.bot_id
            AND visitor
        )
        GROUP BY bot_id;
    """
  end
end
