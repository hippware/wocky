defmodule Wocky.Repo.Migrations.AddBotTables do
  use Wocky.Repo.Migration

  def change do
    create table(:bots, primary_key: false) do
      add :id,               :uuid, primary_key: true
      add :server,           :string, null: false
      add :pending,          :boolean, null: false, default: false
      add :title,            :string
      add :shortname,        :string
      add :user_id,          references(:users, type: :uuid, on_delete: :delete_all), null: false
      add :description,      :string
      add :image,            :string
      add :type,             :string
      add :address,          :string
      add :lat,              :float
      add :lon,              :float
      add :radius,           :integer, null: false, default: 0
      add :public,           :boolean, null: false, default: false
      add :alerts,           :boolean, null: false, default: false
      add :follow_me,        :boolean, null: false, default: false
      add :follow_me_expiry, :integer
      add :tags,             {:array, :string}, null: false, default: []

      timestamps()
    end

    create unique_index(:bots, [:shortname])

    create table(:bot_subscriptions, primary_key: false) do
      add :user_id, references(:users, type: :uuid, on_delete: :delete_all), primary_key: true
      add :bot_id,  references(:bots, type: :uuid, on_delete: :delete_all), primary_key: true

      timestamps()
    end
  end
end
