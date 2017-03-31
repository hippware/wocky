defmodule Wocky.Repo.Migrations.AddDevicesTable do
  use Wocky.Repo.Migration

  def change do
    create table(:devices, primary_key: false) do
      add :user_id,    references(:users, type: :uuid, on_delete: :delete_all), primary_key: true
      add :resource,   :string, null: false, primary_key: true
      add :platform,   :string, null: false
      add :device,     :string, null: false
      add :endpoint,   :text, null: false

      timestamps()
    end
  end
end
