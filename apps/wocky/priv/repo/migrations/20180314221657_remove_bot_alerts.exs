defmodule Wocky.Repo.Migrations.RemoveBotAlerts do
  use Ecto.Migration

  def change do
    alter table(:bots) do
      remove :alerts
    end
  end
end
