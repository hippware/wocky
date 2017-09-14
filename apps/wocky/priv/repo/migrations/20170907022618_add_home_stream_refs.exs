defmodule Wocky.Repo.Migrations.AddHomeStreamRefs do
  use Ecto.Migration

  def change do
    alter table(:home_stream_items) do
      add :reference_user_id, references(:users, type: :uuid, on_delete: :nothing)
      add :reference_bot_id, references(:bots, type: :uuid, on_delete: :nothing)
    end

    create index(:home_stream_items, [:reference_user_id])
    create index(:home_stream_items, [:reference_bot_id])
  end
end
