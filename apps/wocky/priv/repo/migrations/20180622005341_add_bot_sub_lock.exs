defmodule Wocky.Repo.Migrations.AddBotSubLock do
  use Ecto.Migration

  @events ["insert", "update", "delete"]

  def up do
    execute """
    CREATE FUNCTION insert_bot_subscribers_before_trigger()
    RETURNS trigger AS $$
    BEGIN
      PERFORM * FROM bots WHERE id = NEW.bot_id FOR UPDATE;
      RETURN NEW;
    END;
    $$ LANGUAGE plpgsql;
    """

    execute """
    CREATE FUNCTION update_bot_subscribers_before_trigger()
    RETURNS trigger AS $$
    BEGIN
      PERFORM * FROM bots WHERE id = NEW.bot_id FOR UPDATE;
      RETURN NEW;
    END;
    $$ LANGUAGE plpgsql;
    """

    execute """
    CREATE FUNCTION delete_bot_subscribers_before_trigger()
    RETURNS trigger AS $$
    BEGIN
      PERFORM * FROM bots WHERE id = OLD.bot_id FOR UPDATE;
      RETURN OLD;
    END;
    $$ LANGUAGE plpgsql;
    """

    execute """
    CREATE OR REPLACE FUNCTION update_bot_subscribers(updated_bot_id uuid)
    RETURNS void AS $$
    DECLARE
      shash text;
      scount integer;
      ghash text;
      gcount integer;
      vhash text;
      vcount integer;
    BEGIN
      PERFORM * FROM bots  WHERE id = updated_bot_id FOR UPDATE;
      SELECT md5(string_agg(user_id::text, '')), COUNT(user_id)
        INTO shash, scount
        FROM bot_subscriptions
        WHERE bot_id = updated_bot_id;
      SELECT md5(string_agg(user_id::text, '')), COUNT(user_id)
        INTO ghash, gcount
        FROM bot_subscriptions
        WHERE bot_id = updated_bot_id AND guest = true;
      SELECT md5(string_agg(user_id::text, '')), COUNT(user_id)
        INTO vhash, vcount
        FROM bot_subscriptions
        WHERE bot_id = updated_bot_id AND visitor = true;
      IF shash IS NULL THEN
        shash := md5('');
      END IF;
      IF ghash IS NULL THEN
        ghash := md5('');
      END IF;
      IF vhash IS NULL THEN
        vhash := md5('');
      END IF;

      UPDATE bots SET
        subscribers_hash = shash,
        subscribers_count = scount,
        guests_hash = ghash,
        guests_count = gcount,
        visitors_hash = vhash,
        visitors_count = vcount
        WHERE id = updated_bot_id;
    END;
    $$ LANGUAGE plpgsql;
    """

    Enum.each(@events, &create_trigger(&1))
  end

  defp create_trigger(event) do
    execute """
    CREATE TRIGGER #{event}_bot_subscribers_before_trigger BEFORE
      #{String.upcase(event)} ON bot_subscriptions
      FOR EACH ROW
      EXECUTE PROCEDURE #{event}_bot_subscribers_before_trigger();
    """
  end
end
