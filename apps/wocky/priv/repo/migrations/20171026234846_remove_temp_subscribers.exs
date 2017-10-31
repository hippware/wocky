defmodule Wocky.Repo.Migrations.RemoveTempSubscribers do
  use Ecto.Migration

  def change do
    drop table("bot_temp_subscriptions")
  end
end
