defmodule Wocky.Repo.Migrations.AddPushTokenFields do
  use Ecto.Migration

  def change do
    alter table(:push_tokens) do
      add :platform, :string, default: "apns", null: false
      add :dev_mode, :boolean, default: false, null: false
    end
  end
end
