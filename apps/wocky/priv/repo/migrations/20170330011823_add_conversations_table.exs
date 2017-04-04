defmodule Wocky.Repo.Migrations.AddConversationsTable do
  use Wocky.Repo.Migration

  def change do
    create table(:conversations) do
      add :user_id,   references(:users, type: :uuid, on_delete: :delete_all)
      add :other_jid, :string, null: false
      add :message,   :binary, null: false
      add :outgoing,  :boolean, null: false

      timestamps()
    end

    create unique_index(:conversations, [:user_id, :other_jid])
  end

end
