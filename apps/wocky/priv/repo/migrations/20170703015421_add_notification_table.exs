# credo:disable-for-this-file Credo.Check.Readability.MaxLineLength
defmodule Wocky.Repo.Migrations.AddNotificationTable do
  use Wocky.Repo.Migration

  def change do
    create table(:notification_logs) do
      add :user_id,   references(:users, type: :uuid, on_delete: :delete_all), null: false
      add :resource,  :string, null: false
      add :message,   :string, null: false
      add :reference, :binary, null: false
      add :result,    :boolean
      add :details,   :string

      timestamps()
    end

    create index(:notification_logs, :reference)
  end
end
