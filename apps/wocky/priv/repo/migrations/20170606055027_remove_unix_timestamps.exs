defmodule Wocky.Repo.Migrations.RemoveUnixTimestamps do
  use Wocky.Repo.Migration

  def change do
    alter table(:tokens) do
      remove :expires_at
      add :expires_at, :utc_datetime, null: false, default: "1970-01-01 00:00:00"
    end

    alter table(:bots) do
      remove :follow_me_expiry
      add :follow_me_expiry, :utc_datetime
    end
  end
end
