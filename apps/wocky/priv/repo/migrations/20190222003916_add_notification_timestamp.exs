defmodule Wocky.Repo.Migrations.AddNotificationTimestamp do
  use Wocky.Repo.Migration

  def change do
    alter table(:notifications) do
      add :expires_at, :utc_datetime
    end
  end
end
