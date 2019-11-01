defmodule Wocky.Repo.Migrations.NearbyFields do
  use Wocky.Repo.Migration

  alias Wocky.Notifier.InBand.Notification.NotificationTypeEnum

  def up do
    alter table("roster_items") do
      add :nearby_distance, :integer, default: 2000, null: false
      add :nearby_cooldown, :bigint, default: :timer.hours(24), null: false
      add :nearby_last_start_notification, :timestamptz
    end

    Migration.reset_enum(NotificationTypeEnum, [{"notifications", "type", "bot_invitation"}])
  end
end
