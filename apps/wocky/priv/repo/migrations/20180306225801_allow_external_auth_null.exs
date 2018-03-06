defmodule Wocky.Repo.Migrations.AllowExternalAuthNull do
  use Wocky.Repo.Migration

  def change do
    alter table(:users) do
      modify :provider,     :string, null: true
      modify :external_id, :string, null: true
    end
  end
end
