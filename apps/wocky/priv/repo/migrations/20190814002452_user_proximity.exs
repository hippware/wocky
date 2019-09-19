defmodule Wocky.Repo.Migrations.UserProximity do
  use Wocky.Repo.Migration

  import DawdleDB.Migration

  alias Wocky.Notifier.InBand.Notification.NotificationTypeEnum

  @disable_ddl_transaction true

  def up do
    create table("user_proximity_subscriptions", primary_key: false) do
      add :user_id, references(:users, on_delete: :delete_all, type: :uuid), null: false, primary_key: true
      add :target_id, references(:users, on_delete: :delete_all, type: :uuid), null: false, primary_key: true
      add :range, :integer, default: 1000, null: false
      add :cooldown, :bigint, default: :timer.hours(24), null: false
      add :last_notification, :timestamptz

      timestamps()
    end

    create index("user_proximity_subscriptions", [:user_id])
    create index("user_proximity_subscriptions", [:target_id])

    update_notify("user_proximity_subscriptions", [:insert, :delete, :update])

    Migration.reset_enum(NotificationTypeEnum, [{"notifications", "type", "bot_invitation"}])
  end
end
