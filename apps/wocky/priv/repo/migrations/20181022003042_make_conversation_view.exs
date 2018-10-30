defmodule Wocky.Repo.Migrations.MakeConversationView do
  use Wocky.Repo.Migration

  def up do
    drop table(:conversations)

    execute """
    CREATE OR REPLACE FUNCTION min(uuid, uuid)
    RETURNS uuid AS $$
    BEGIN
        IF $2 IS NULL OR $1 > $2 THEN
            RETURN $2;
        END IF;

        RETURN $1;
    END;
    $$ LANGUAGE plpgsql;
    """

    execute """
    CREATE OR REPLACE FUNCTION max(uuid, uuid)
    RETURNS uuid AS $$
    BEGIN
        IF $2 IS NULL OR $1 > $2 THEN
            RETURN $1;
        END IF;

        RETURN $2;
    END;
    $$ LANGUAGE plpgsql;
    """

    execute """
    CREATE VIEW conversations
      (id, user_id, other_user_id, message, created_at, direction) AS
      (
        SELECT DISTINCT ON (
          min(sender_id, recipient_id),
          max(sender_id, recipient_id)
        )
          id,
          sender_id,
          recipient_id,
          message,
          created_at,
          'outgoing'
        FROM messages
        GROUP BY created_at, id, sender_id, recipient_id
        ORDER BY
          min(sender_id, recipient_id),
          max(sender_id, recipient_id),
          created_at desc
      )
      UNION ALL
      (
        SELECT DISTINCT ON (
          min(sender_id, recipient_id),
          max(sender_id, recipient_id)
        )
          id,
          recipient_id,
          sender_id,
          message,
          created_at,
          'incoming'
        FROM messages
        GROUP BY created_at, id, sender_id, recipient_id
        ORDER BY
          min(sender_id, recipient_id),
          max(sender_id, recipient_id),
          created_at desc
      )
    """
  end
end
