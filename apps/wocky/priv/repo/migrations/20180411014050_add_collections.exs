defmodule Wocky.Repo.Migrations.AddCollections do
  use Wocky.Repo.Migration

  def change do
    create table(:collections) do
      add :user_id,     references(:users, type: :uuid, on_delete: :delete_all)
      add :title,       :string

      timestamps()
    end

    create table(:collection_members, primary_key: false) do
      add :collection_id,
        references(:collections, on_delete: :delete_all),
        primary_key: true
      add :bot_id,
        references(:bots, type: :uuid, on_delete: :delete_all),
        primary_key: true

      timestamps()
    end

    create table(:collection_subscriptions, primary_key: false) do
      add :collection_id,
        references(:collections, on_delete: :delete_all),
        primary_key: true
      add :user_id,
        references(:users, type: :uuid, on_delete: :delete_all),
        primary_key: true

      timestamps()
    end
  end
end
