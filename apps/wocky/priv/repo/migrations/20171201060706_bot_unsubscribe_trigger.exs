defmodule Wocky.Repo.Migrations.BotUnsubscribeTrigger do
  use Wocky.Repo.Migration

  def up do
    execute """
    CREATE FUNCTION update_bot_public_trigger()
    RETURNS trigger AS $$
    BEGIN
      IF NEW.public = false AND OLD.public = true THEN
        DELETE FROM bot_subscriptions WHERE bot_id = NEW.id
          AND NOT is_visible(user_id, NEW);
      END IF;
      RETURN NULL;
    END;
    $$ LANGUAGE plpgsql;
    """

    execute """
    CREATE TRIGGER update_bot_public_trigger AFTER
      UPDATE ON bots
      FOR EACH ROW
      EXECUTE PROCEDURE update_bot_public_trigger();
    """
  end

  def down do
    execute "DROP TRIGGER update_bot_public_trigger on bots"
    execute "DROP FUNCTION update_bot_public_trigger()"
  end
end
