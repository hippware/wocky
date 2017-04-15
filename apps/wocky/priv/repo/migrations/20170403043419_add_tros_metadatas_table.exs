defmodule Wocky.Repo.Migrations.AddTROSMetadatasTable do
  use Wocky.Repo.Migration

  def change do
    create table(:tros_metadatas, primary_key: false) do
      add :id,      :uuid, null: false, primary_key: true
      add :user_id, references(:users, type: :uuid, on_delete: :delete_all), null: false
      add :access,  :binary, null: false

      timestamps()
    end
  end
end
