defmodule Wocky.Repo.Migrations.AddBotTables do
  use Wocky.Repo.Migration

  def change do
    create table(:bots, primary_key: false) do
      add :id,               :uuid, primary_key: true
      add :server,           :string, null: false
      add :title,            :string
      add :shortname,        :string
      add :user_id,          references(:users, type: :uuid, on_delete: :delete_all), null: false
      add :description,      :string
      add :image,            :string
      add :type,             :string
      add :address,          :string
      add :lat,              :float, null: false
      add :lon,              :float, null: false
      add :radius,           :integer, null: false
      add :visibility,       :integer
      add :alerts,           :boolean, null: false, default: true
      add :follow_me,        :boolean, null: false, default: false
      add :follow_me_expiry, :integer

      timestamps()
    end

    create table(:bot_subscribers, primary_key: false) do
      add :user_id, references(:users, type: :uuid, on_delete: :delete_all), null: false
      add :bot_id,  references(:bots, type: :uuid, on_delete: :delete_all), null: false

      timestamps()
    end
  end
end
