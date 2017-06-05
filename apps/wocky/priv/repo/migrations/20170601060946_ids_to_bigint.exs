defmodule Wocky.Repo.Migrations.IdsToBigint do
  use Ecto.Migration

  def change do
    alter table(:conversations) do
      modify :id, :bigint
    end

    alter table(:home_stream_items) do
      modify :id, :bigint
    end

    alter table(:roster_items) do
      modify :id, :bigint
    end

    alter table(:traffic_logs) do
      modify :id, :bigint
    end
  end
end
