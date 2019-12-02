defmodule Wocky.Repo.Migrations.AddLocShareEndSelfNotification do
  use Wocky.Repo.Migration

  alias Wocky.Notifier.InBand.Notification.NotificationTypeEnum

  @disable_ddl_transaction true

  def up do
    alter table(:notifications) do
      add :share_id, :bigint
    end

    reset_enum(NotificationTypeEnum, [
      {"notifications", "type", "bot_invitation"}
    ])
  end
end
