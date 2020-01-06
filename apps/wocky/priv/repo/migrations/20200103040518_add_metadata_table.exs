defmodule Wocky.Repo.Migrations.AddMetadataTable do
  use Ecto.Migration

  def change do
    create table("metadata", primary_key: false) do
      add :key, :string, null: false, primary_key: true
      add :value, :string
      add :description, :string
    end
  end
end
