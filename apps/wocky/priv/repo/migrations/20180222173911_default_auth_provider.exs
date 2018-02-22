defmodule Wocky.Repo.Migrations.DefaultAuthProvider do
  use Ecto.Migration

  def change do
    alter table(:users) do
      modify :provider, :string, null: false, default: "firebase"
    end
  end
end
