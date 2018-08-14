defmodule Wocky.Repo.Migrations.BotEventOccurredAt do
  use Ecto.Migration

  def change do
    rename table(:user_bot_events), :updated_at, to: :occurred_at
  end
end
