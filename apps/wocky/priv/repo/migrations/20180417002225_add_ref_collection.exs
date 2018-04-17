defmodule Wocky.Repo.Migrations.AddRefCollection do
  use Wocky.Repo.Migration

  def change do
    alter table(:home_stream_items) do
      add :reference_collection_id, references(:collections, on_delete: :nothing)
    end

    create index(:home_stream_items, [:reference_collection_id])
  end
end
