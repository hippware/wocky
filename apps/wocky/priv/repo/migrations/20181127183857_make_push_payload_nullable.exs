defmodule Wocky.Repo.Migrations.MakePushPayloadNullable do
  use Ecto.Migration

  def change do
    alter table(:push_logs) do
      modify :payload, :text, null: true
    end
  end
end
