defmodule Wocky.Repo.Migrations.AddFileMetadataTable do
  use Wocky.Repo.Migration

  def change do
    create table(:tros_metadata, primary_key: false) do
      add :id,      :uuid, null: false, primary_key: true
      add :user_id, references(:users, type: :uuid, on_delete: :delete_all)
      add :access,  :binary, null: false

      timestamps()
    end
  end
end
