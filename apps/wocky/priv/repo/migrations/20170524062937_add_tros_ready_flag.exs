defmodule Wocky.Repo.Migrations.AddTrosReadyFlag do
  use Ecto.Migration

  def change do
    alter table(:tros_metadatas) do
      add :ready, :boolean, default: true
    end
  end
end
