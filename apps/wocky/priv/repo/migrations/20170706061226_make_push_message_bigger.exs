defmodule Wocky.Repo.Migrations.MakePushMessageBigger do
  use Wocky.Repo.Migration

  def change do
    alter table(:notification_logs) do
      modify :message, :text, null: false
    end
  end
end
