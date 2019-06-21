defmodule Wocky.Repo.Migrations.ConversationAddClientData do
  use Wocky.Repo.Migration

  def up do
    execute """
    DROP VIEW conversations
    """

    execute """
    CREATE VIEW conversations
    (
      id,
      user_id,
      other_user_id,
      content,
      image_url,
      client_data,
      created_at,
      direction
    ) AS
      (
        SELECT DISTINCT ON (
          min(sender_id, recipient_id),
          max(sender_id, recipient_id)
        )
          id,
          sender_id,
          recipient_id,
          content,
          image_url,
          client_data,
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
          content,
          image_url,
          client_data,
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
