defmodule Wocky.Repo.Migrations.AddBlockingTable do
  use Wocky.Repo.Migration

  def change do
    create table(:blocks, primary_key: false) do
      add :blocker_id, references(:users, type: :uuid, on_delete: :delete_all), primary_key: true
      add :blockee_id, references(:users, type: :uuid, on_delete: :delete_all), primary_key: true

      timestamps()
    end

    create index("blocks", [:blocker_id])
    create index("blocks", [:blockee_id])
  end
end
