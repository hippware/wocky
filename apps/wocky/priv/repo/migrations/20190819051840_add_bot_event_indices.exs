defmodule Wocky.Repo.Migrations.AddBotEventIndices do
  use Wocky.Repo.Migration

  def change do
    create index("user_bot_events", [:user_id])
    create index("user_bot_events", [:bot_id])
  end
end
