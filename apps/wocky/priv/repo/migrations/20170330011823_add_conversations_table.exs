defmodule Wocky.Repo.Migrations.AddConversationsTable do
  use Wocky.Repo.Migration

  def change do
    create table(:conversations, primary_key: false) do
      add :user,      :string, primary_key: true
      add :other_jid, :string, primary_key: true
      add :message,   :binary
      add :outgoing,  :boolean

      timestamps()
    end
  end

end
