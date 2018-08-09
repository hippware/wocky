defmodule Wocky.Repo.Migrations.BotEventOccurredAt do
  use Ecto.Migration

  def up do
    alter table(:user_bot_events) do
      add :occurred_at, :timestamptz, null: false
      remove :updated_at
    end

    execute "update user_bot_events set occurred_at = created_at;"
  end

  def down do
    alter table(:user_bot_events) do
      add :updated_at, :timestamptz, null: false
      remove :occurred_at
    end

    execute "update user_bot_events set updated_at = created_at;"
  end
end
