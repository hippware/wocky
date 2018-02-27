defmodule Wocky.Repo.Migrations.AddHsDeleteTriggers do
  use Wocky.Repo.Migration

  def up do
    add_delete_trigger("users", "reference_user_id")
    add_delete_trigger("bots", "reference_bot_id")
    add_delete_trigger("bot_items", "reference_bot_item_id")

    recreate_bot_private_trigger()
  end

  defp add_delete_trigger(table, field) do
    execute """
    CREATE FUNCTION delete_from_#{table}_trigger()
    RETURNS trigger AS $$
    BEGIN
      #{set_hs_item_deleted()}
      WHERE
        #{field} = OLD.id
        #{maybe_extra_condition(table)};
      RETURN OLD;
    END;
    $$ LANGUAGE plpgsql;
    """

    execute """
    CREATE TRIGGER delete_from_#{table}_trigger BEFORE
      DELETE ON #{table}
      FOR EACH ROW
      EXECUTE PROCEDURE delete_from_#{table}_trigger();
    """
  end

  defp maybe_extra_condition("bot_items") do
    "AND reference_bot_id = OLD.bot_id"
  end
  defp maybe_extra_condition(_), do: ""

  defp set_hs_item_deleted do
    """
      UPDATE home_stream_items SET
        class = 'deleted',
        stanza = '',
        from_jid = '',
        reference_user_id = null,
        reference_bot_id = null,
        reference_bot_item_id = null,
        updated_at = now()
    """
  end

  defp recreate_bot_private_trigger do
    execute "DROP TRIGGER update_bot_public_trigger on bots"
    execute "DROP FUNCTION update_bot_public_trigger()"

    execute """
    CREATE FUNCTION update_bot_private_trigger()
    RETURNS trigger AS $$
    BEGIN
      IF NEW.public = false AND OLD.public = true THEN
        DELETE FROM bot_subscriptions WHERE bot_id = NEW.id
          AND NOT is_visible(user_id, NEW);
        #{set_hs_item_deleted()}
        WHERE
          reference_bot_id = NEW.id
          AND NOT is_visible(user_id, NEW);
      END IF;
      RETURN NEW;
    END;
    $$ LANGUAGE plpgsql;
    """

    # This is necessarily an AFTER trigger because changing the subscriptions
    # feeds back into changing the bot's subscriber hash. A BEFORE would get
    # us into a loop of triggers (which postgres thankfully catches for us).
    execute """
    CREATE TRIGGER update_bot_private_trigger AFTER
      UPDATE ON bots
      FOR EACH ROW
      EXECUTE PROCEDURE update_bot_private_trigger();
    """
  end
end
