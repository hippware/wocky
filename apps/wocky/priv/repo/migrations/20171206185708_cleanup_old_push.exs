# credo:disable-for-this-file Credo.Check.Readability.MaxLineLength
defmodule Wocky.Repo.Migrations.CleanupOldPush do
  use Wocky.Repo.Migration

  def up do
    drop table(:notification_logs)
    drop table(:devices)
  end

  def down do
    create table(:notification_logs) do
      add :user_id,   references(:users, type: :uuid, on_delete: :delete_all), null: false
      add :resource,  :string, null: false
      add :message,   :text, null: false
      add :reference, :binary, null: false
      add :result,    :boolean
      add :details,   :string

      timestamps()
    end

    create index(:notification_logs, :reference)
    create index(:notification_logs, [:created_at])

    create table(:devices, primary_key: false) do
      add :user_id,    references(:users, type: :uuid, on_delete: :delete_all), primary_key: true
      add :resource,   :string, null: false, primary_key: true
      add :platform,   :string, null: false
      add :token,      :string, null: false
      add :invalid,    :boolean, null: false, default: false
      add :feedback,   :boolean, null: false, default: false

      timestamps()
    end

    create index(:devices, [:token])
  end
end
