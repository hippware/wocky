defmodule Wocky.Repo.Migrations.AddAuthProvider do
  use Ecto.Migration

  def change do
    alter table(:users) do
      add :auth_provider, :string, null: false, default: "digits"
    end
  end
end
