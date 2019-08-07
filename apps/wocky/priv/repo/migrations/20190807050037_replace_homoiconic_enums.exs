defmodule Wocky.Repo.Migrations.ReplaceHomoiconicEnums do
  # credo:disable-for-this-file Credo.Check.Warning.UnsafeToAtom

  use Wocky.Repo.Migration

  @disable_ddl_transaction true

  alias Wocky.Location.BotEvent.EventTypeEnum
  alias Wocky.Messaging.Conversation.MessageDirectionEnum
  alias Wocky.Notifier.InBand.Notification.GeofenceEventTypeEnum
  alias Wocky.Notifier.InBand.Notification.NotificationTypeEnum
  alias Wocky.Notifier.Push.Token.PushServicePlatformEnum

  def up do
    convert("user_bot_events", "event", EventTypeEnum)
    convert("notifications", "geofence_event", GeofenceEventTypeEnum)
    convert("notifications", "type", NotificationTypeEnum, false,
      fragment("'bot_invitation'::#{NotificationTypeEnum.type()}"))
    convert("push_tokens", "platform", PushServicePlatformEnum, false,
      fragment("'apns'::#{PushServicePlatformEnum.type()}"))
    update_conversation_view()
  end

  defp convert(table, source, type, null \\ true, default \\ nil) do
    type.create_type()

    tmp = String.to_atom(source <> "_tmp")

    alter table(table) do
      add tmp, type.type(), null: null, default: default
    end

    execute """
    UPDATE #{table} SET #{source}_tmp = #{source}::#{type.type()}
    """

    alter table(table) do
      remove String.to_atom(source)
    end

    rename table(table), tmp, to: String.to_atom(source)
  end

  defp update_conversation_view do
    MessageDirectionEnum.create_type()

    execute """
    DROP VIEW IF EXISTS conversations
    """

    execute """
    CREATE VIEW conversations AS ( SELECT DISTINCT ON ((min(messages.sender_id, messages.recipient_id)), (max(messages.sender_id, messages.recipient_id))) messages.id,
        messages.sender_id AS user_id,
        messages.recipient_id AS other_user_id,
        messages.content,
        messages.image_url,
        messages.client_data,
        messages.read,
        messages.created_at,
        messages.updated_at,
        'outgoing'::message_direction AS direction
       FROM messages
      GROUP BY messages.created_at, messages.id, messages.sender_id, messages.recipient_id
      ORDER BY (min(messages.sender_id, messages.recipient_id)), (max(messages.sender_id, messages.recipient_id)), messages.created_at DESC)
    UNION ALL
    ( SELECT DISTINCT ON ((min(messages.sender_id, messages.recipient_id)), (max(messages.sender_id, messages.recipient_id))) messages.id,
        messages.recipient_id AS user_id,
        messages.sender_id AS other_user_id,
        messages.content,
        messages.image_url,
        messages.client_data,
        messages.read,
        messages.created_at,
        messages.updated_at,
        'incoming'::message_direction AS direction
       FROM messages
      GROUP BY messages.created_at, messages.id, messages.sender_id, messages.recipient_id
      ORDER BY (min(messages.sender_id, messages.recipient_id)), (max(messages.sender_id, messages.recipient_id)), messages.created_at DESC);
    """
  end
end
