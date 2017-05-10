defmodule Wocky.Repo.Migrations.AddHomeStreamTable do
  use Wocky.Repo.Migration

  def change do
    create table(:home_stream_items) do
      add :user_id,   references(:users, type: :uuid, on_delete: :delete_all)
      add :key,       :string, null: false
      add :from_jid,  :string, null: false
      add :stanza,    :binary, null: false
      add :deleted,   :boolean, null: false

      timestamps()
    end

    create unique_index(:home_stream_items, [:user_id, :key])
  end
end
