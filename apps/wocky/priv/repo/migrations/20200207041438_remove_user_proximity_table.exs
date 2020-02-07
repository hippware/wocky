defmodule Wocky.Repo.Migrations.RemoveUserProximityTable do
  use Wocky.Repo.Migration

  alias Wocky.Notifier.InBand.Notification.NotificationTypeEnum

  def change do
    drop table("user_proximity_subscriptions")

    execute "DELETE FROM notifications WHERE type::text = 'user_proximity'"

    reset_enum(NotificationTypeEnum, [
      {"notifications", "type", "bot_invitation"}
    ])
  end
end
