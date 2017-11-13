defmodule Wocky.Repo.Migrations.UTCTimestamps do
  use Wocky.Repo.Migration

  defmacrop do_ts_change(table, type, fields) do
    quote do
      alter unquote(table) do
        unquote(Enum.map(fields, fn field ->
          quote do
            modify unquote(field), unquote(type)
          end
        end))
      end
    end
  end

  defmacrop upgrade(table, fields \\ [:created_at, :updated_at]) do
    quote do
      do_ts_change(unquote(table), :timestamptz, unquote(fields))
    end
  end

  defmacrop downgrade(table, fields \\ [:created_at, :updated_at]) do
    quote do
      do_ts_change(unquote(table), :timestamp, unquote(fields))
    end
  end

  def up do
    upgrade table(:bot_items)
    upgrade table(:bot_shares)
    upgrade table(:bot_subscriptions)
    upgrade table(:bots), [:created_at, :updated_at, :follow_me_expiry]
    upgrade table(:conversations)
    upgrade table(:devices)
    upgrade table(:home_stream_items), [:created_at, :updated_at, :ordering]
    upgrade table(:initial_contacts)
    upgrade table(:notification_logs)
    upgrade table(:privacy_list), [:created_at]
    upgrade table(:private_storage), [:created_at]
    upgrade table(:roster_items)
    upgrade table(:schema_migrations), [:inserted_at]
    upgrade table(:tokens), [:created_at, :updated_at, :expires_at]
    upgrade table(:traffic_logs), [:created_at]
    upgrade table(:tros_metadatas)
    upgrade table(:user_bot_events)
    upgrade table(:user_locations)
    upgrade table(:users)
  end

  def down do
    downgrade table(:bot_items)
    downgrade table(:bot_shares)
    downgrade table(:bot_subscriptions)
    downgrade table(:bots), [:created_at, :updated_at, :follow_me_expiry]
    downgrade table(:conversations)
    downgrade table(:devices)
    downgrade table(:home_stream_items), [:created_at, :updated_at, :ordering]
    downgrade table(:initial_contacts)
    downgrade table(:notification_logs)
    downgrade table(:privacy_list), [:created_at]
    downgrade table(:private_storage), [:created_at]
    downgrade table(:roster_items)
    downgrade table(:schema_migrations), [:inserted_at]
    downgrade table(:tokens), [:created_at, :updated_at, :expires_at]
    downgrade table(:traffic_logs), [:created_at]
    downgrade table(:tros_metadatas)
    downgrade table(:user_bot_events)
    downgrade table(:user_locations)
    downgrade table(:users)
  end
end
