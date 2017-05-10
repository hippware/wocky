defmodule Wocky.Repo.Migrations.AddTokensTable do
  use Wocky.Repo.Migration

  def change do
    create table(:tokens, primary_key: false) do
      add :user_id,    references(:users, type: :uuid, on_delete: :delete_all), primary_key: true
      add :resource,   :string, null: false, primary_key: true
      add :token,      :string, null: false
      add :expires_at, :integer, null: false

      timestamps()
    end
  end
end
