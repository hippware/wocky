defmodule Wocky.Repo.Migrations.RemoveAuthTokens do
  use Wocky.Repo.Migration

  def up do
    drop table(:tokens)
  end

  def down do
    create table(:tokens, primary_key: false) do
      add :user_id,    references(:users, type: :uuid, on_delete: :delete_all), primary_key: true
      add :device,     :string, null: false, primary_key: true
      add :token_hash, :string, null: false
      add :expires_at, :utc_datetime, null: false, default: "1970-01-01 00:00:00"

      timestamps()
    end
  end
end
