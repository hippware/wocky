defmodule Wocky.Repo.Migrations.AutoCalculateSubHash do
  use Wocky.Repo.Migration

  @events ["insert", "update", "delete", "truncate"]

  def up do
    alter table(:bots) do
      add :subscribers_hash, :text,
        default: "d41d8cd98f00b204e9800998ecf8427e", # md5("")
        null: false
      add :subscribers_count, :integer, default: 0, null: false
    end

    flush()

    # Create a function to pre-calculate subscriber hash and count
    execute """
    CREATE FUNCTION update_bot_subscribers(updated_bot_id uuid)
    RETURNS void AS $$
    DECLARE
      shash text;
      scount integer ;
    BEGIN
      SELECT md5(string_agg(user_id::text, '')), COUNT(user_id)
        INTO shash, scount
        FROM bot_subscriptions
        WHERE bot_id = updated_bot_id;
      IF shash IS NULL THEN
        shash := md5('');
      END IF;
      UPDATE bots SET
        subscribers_hash = shash,
        subscribers_count = scount
        WHERE id = updated_bot_id;
    END;
    $$ LANGUAGE plpgsql;
    """

    execute """
    CREATE FUNCTION insert_bot_subscribers_trigger()
    RETURNS trigger AS $$
    BEGIN
        PERFORM update_bot_subscribers(NEW.bot_id);
      RETURN NULL;
    END;
    $$ LANGUAGE plpgsql;
    """

    execute """
    CREATE FUNCTION update_bot_subscribers_trigger()
    RETURNS trigger AS $$
    BEGIN
        PERFORM update_bot_subscribers(NEW.bot_id);
        IF NEW.bot_id != OLD.bot_id THEN
          PERFORM update_bot_subscribers(OLD.bot_id);
        END IF;
      RETURN NULL;
    END;
    $$ LANGUAGE plpgsql;
    """

    execute """
    CREATE FUNCTION delete_bot_subscribers_trigger()
    RETURNS trigger AS $$
    BEGIN
      PERFORM update_bot_subscribers(OLD.bot_id);
      RETURN NULL;
    END;
    $$ LANGUAGE plpgsql;
    """

    execute """
    CREATE FUNCTION truncate_bot_subscribers_trigger()
    RETURNS trigger AS $$
    DECLARE bot_id uuid;
    BEGIN
      FOR bot_id IN (SELECT id FROM bots) LOOP
        PERFORM update_bot_subscribers(bot_id);
      END LOOP;
      RETURN NULL;
    END;
    $$ LANGUAGE plpgsql;
    """

    # Run the function for all bots to pre-populate existing data
    execute """
    DO $$
      DECLARE bot_id uuid;
      BEGIN
        FOR bot_id IN (SELECT id FROM bots) LOOP
          PERFORM update_bot_subscribers(bot_id);
        END LOOP;
      END;
    $$
    """

    # Create a trigger to update a bot whenever its subscribers change
    Enum.each(@events, &create_trigger(&1))

  end

  def down do
    Enum.each(@events, &drop_trigger_and_function(&1))

    execute "DROP FUNCTION update_bot_subscribers(updated_bot_id uuid)"

    alter table(:bots) do
      remove :subscribers_hash
      remove :subscribers_count
    end
  end

  defp create_trigger(event) do
    execute """
    CREATE TRIGGER #{event}_bot_subscribers_trigger AFTER
      #{String.upcase(event)} ON bot_subscriptions
      FOR EACH #{foreach(event)}
      EXECUTE PROCEDURE #{event}_bot_subscribers_trigger();
    """
  end

  defp foreach("truncate"), do: "STATEMENT"
  defp foreach(_), do: "ROW"

  defp drop_trigger_and_function(event) do
    execute "DROP TRIGGER #{event}_bot_subscribers_trigger ON bot_subscriptions"
    execute "DROP FUNCTION #{event}_bot_subscribers_trigger()"
  end

end
