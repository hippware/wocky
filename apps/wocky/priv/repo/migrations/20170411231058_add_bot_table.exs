defmodule Wocky.Repo.Migrations.AddBotTable do
  use Ecto.Migration

  def change do
    create table(:bots, primary_key: false) do
      add :id,               :uuid, primary_key: true
      add :server,           :string, null: false
      add :title,            :string
      add :shortname,        :string
      add :user_id,          references(:users, type: :uuid), null: false
      add :description,      :string
      add :image,            :string
      add :type,             :string
      add :address,          :string
      add :lat,              :double, null: false
      add :lon,              :double, null: false
      add :radius,           :integer, null: false
      add :visibility,       :integer
      add :alerts,           :boolean, null: false, default: true
      add :follow_me,        :boolean, null: false, default: false
      add :follow_me_expiry, :integer

      timestamps()
    end
  end
end
