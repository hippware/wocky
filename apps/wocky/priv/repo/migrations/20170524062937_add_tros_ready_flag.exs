defmodule Wocky.Repo.Migrations.AddTrosReadyFlag do
  use Wocky.Repo.Migration

  def change do
    alter table(:tros_metadatas) do
      add :ready, :boolean, default: true
    end
  end
end
