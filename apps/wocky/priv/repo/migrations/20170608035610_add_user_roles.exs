defmodule Wocky.Repo.Migrations.AddUserRoles do
  use Wocky.Repo.Migration

  def change do
    alter table(:users) do
      add :roles, {:array, :string}, null: false, default: []
    end
  end
end
