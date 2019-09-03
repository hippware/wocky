defmodule Wocky.Repo.Migrations.DataConsistencyFuns do
  use Wocky.Repo.Migration

  alias DawdleDB.Migration

  def up do
    # Bot owner subscription
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

    # Roster item deletion
    execute """
    CREATE FUNCTION roster_item_delete_trigger()
    RETURNS trigger AS $$
    BEGIN
      PERFORM remove_all_relationships(OLD.user_id, OLD.contact_id);
      PERFORM remove_all_relationships(OLD.contact_id, OLD.user_id);
      RETURN NULL;
    END;
    $$ LANGUAGE plpgsql
    """

    execute """
    CREATE TRIGGER roster_item_delete_trigger
    AFTER DELETE ON roster_items
    FOR EACH ROW
    EXECUTE PROCEDURE roster_item_delete_trigger();
    """

    # Blocking
    execute """
    CREATE FUNCTION block_insert_trigger()
    RETURNS trigger AS $$
    BEGIN
      PERFORM block_user(NEW.blocker_id, NEW.blockee_id);
      PERFORM block_user(NEW.blockee_id, NEW.blocker_id);
      RETURN NULL;
    END;
    $$ LANGUAGE plpgsql
    """

    execute """
    CREATE TRIGGER block_insert_trigger
    AFTER INSERT ON blocks
    FOR EACH ROW
    EXECUTE PROCEDURE block_insert_trigger();
    """

    Migration.remove_notify("blocks", :insert)

    # Helpers
    execute """
    CREATE FUNCTION remove_all_relationships(UUID, UUID)
    RETURNS void AS $$
    BEGIN
      DELETE FROM user_location_shares WHERE user_id = $1 AND shared_with_id = $2;

      DELETE FROM bot_subscriptions S USING bots B
      WHERE S.bot_id = B.id
        AND B.user_id = $1
        AND S.user_id = $2;

      DELETE FROM bot_invitations WHERE user_id = $1 AND invitee_id = $2;

      DELETE FROM user_invitations WHERE user_id = $1 AND invitee_id = $2;
    END;
    $$ LANGUAGE plpgsql
    """

    execute """
    CREATE FUNCTION block_user(UUID, UUID)
    RETURNS void AS $$
    BEGIN
      PERFORM remove_all_relationships($1, $2);

      DELETE FROM bot_items I USING bots B
      WHERE I.bot_id = B.id
        AND B.user_id = $1
        AND I.user_id = $2;

      DELETE FROM roster_items WHERE user_id = $1 AND contact_id = $2;

      DELETE FROM notifications WHERE user_id = $1 AND other_user_id = $2;
      DELETE FROM notifications N USING bots B
        WHERE N.bot_id = B.id
        AND B.user_id = $1 AND N.user_id = $2;
    END;
    $$ LANGUAGE plpgsql
    """
  end

  def down do
    execute "DROP FUNCTION bot_insert_trigger"
    execute "DROP TRIGGER bot_insert_trigger"

    execute "DROP FUNCTION roster_item_delete_trigger"
    execute "DROP TRIGGER roster_item_delete_trigger"

    execute "DROP FUNCTION block_isnert_trigger"
    execute "DROP TRIGGER block_isnert_trigger"

    execute "DROP FUNCTION remove_all_relationships(uuid, uuid)"
    execute "DROP FUNCTION block_user(uuid, uuid)"

    Migration.update_notify("bots", :insert)
    Migration.update_notify("roster_items", :delete)
    Migration.update_notify("blocks", :insert)
  end
end
