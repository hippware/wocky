defmodule Wocky.Repo.Migrations.AddInitialFollowees do
  use Wocky.Repo.Migration

  def change do
    create table(:initial_followees, primary_key: false) do
      add :user_id, references(:users, type: :uuid, on_delete: :delete_all), primary_key: true

      timestamps()
    end
  end
end
