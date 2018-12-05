defmodule Wocky.Repo.Migrations.AddClientVersionsTable do
  use Wocky.Repo.Migration

  def change do
    create table(:client_versions, primary_key: false) do
      add :user_id,    references(:users, type: :uuid, on_delete: :delete_all), primary_key: true
      add :device,     :string, null: false, primary_key: true
      add :version,    :string, null: false
      add :attributes, {:array, :string}, null: false, default: []

      timestamps()
    end
  end
end
