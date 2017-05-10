defmodule Wocky.Repo.Migrations.MoreBotTables do
  use Wocky.Repo.Migration

  def change do
    create table(:bot_temp_subscriptions, primary_key: false) do
      add :bot_id, references(:bots, type: :uuid, on_delete: :delete_all), primary_key: true
      add :user_id, references(:users, type: :uuid, on_delete: :delete_all), primary_key: true
      add :resource, :string, primary_key: true
      add :node, :string, null: false

      timestamps()
    end

    create index(:bot_temp_subscriptions, [:node])

    create table(:bot_shares, primary_key: false) do
      add :bot_id, references(:bots, type: :uuid, on_delete: :delete_all), primary_key: true
      add :user_id, references(:users, type: :uuid, on_delete: :delete_all), primary_key: true
      add :sharer_id, references(:users, type: :uuid, on_delete: :nilify_all)

      timestamps()
    end

    create table(:bot_items, primary_key: false) do
      add :id,     :string, primary_key: true
      add :bot_id, references(:bots, type: :uuid, on_delete: :delete_all), primary_key: true
      add :stanza, :text, null: false
      add :image,  :boolean, null: false, default: false

      timestamps()
    end
  end
end
