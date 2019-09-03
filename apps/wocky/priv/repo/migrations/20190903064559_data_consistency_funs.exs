defmodule Wocky.Repo.Migrations.DataConsistencyFuns do
  use Wocky.Repo.Migration

  alias DawdleDB.Migration

  def up do
    execute """
    CREATE FUNCTION bot_insert_trigger()
    RETURNS trigger AS $$
    BEGIN
      INSERT INTO bot_subscriptions (
        user_id,
        bot_id,
        created_at,
        updated_at
      )
      VALUES (
        NEW.user_id,
        NEW.id,
        now(),
        now()
      );
      RETURN NULL;
    END;
    $$ LANGUAGE plpgsql
    """

    execute """
    CREATE TRIGGER bot_insert_trigger
    AFTER INSERT ON bots
    FOR EACH ROW
    EXECUTE PROCEDURE bot_insert_trigger();
    """

    Migration.remove_notify("bots", :insert)
  end

  def down do
    execute "DROP FUNCTION bot_insert_trigger"
    execute "DROP TRIGGER bot_insert_trigger"

    Migration.update_notify("bots", :insert)
  end
end
