defmodule Wocky.Repo.Migrations.AddAuthProvider do
  use Ecto.Migration

  def change do
    alter table(:users) do
      add :provider, :string, null: false, default: "digits"
    end
  end
end
