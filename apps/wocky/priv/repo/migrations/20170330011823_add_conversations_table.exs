defmodule Wocky.Repo.Migrations.AddConversationsTable do
  use Wocky.Repo.Migration

  def up do
    create table(:conversations, primary_key: false) do
      add :server,    :string
      add :user,      :string, primary_key: true
      add :other_jid, :string, primary_key: true
      add :message,   :binary
      add :outgoing,  :boolean

      timestamps()
    end
  end

  def down do
    drop table(:conversations)
  end

end
