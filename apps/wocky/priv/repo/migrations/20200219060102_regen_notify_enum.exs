defmodule Wocky.Repo.Migrations.RegenNotifyEnum do
  use Wocky.Repo.Migration

  alias Wocky.Notifier.InBand.Notification.NotificationTypeEnum

  def change do
    reset_enum(NotificationTypeEnum, [
      {"notifications", "type", "bot_invitation"}
    ])
  end
end
