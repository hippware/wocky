defmodule Wocky.Repo.Migrations.PushLogMsgId do
  use Wocky.Repo.Migration

  def change do
    alter table(:push_logs) do
      modify :message_id, :string, null: true
    end
  end
end
