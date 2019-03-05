defmodule Wocky.Repo.Migrations.AddClientBlob do
  use Ecto.Migration

  def change do
    alter table(:users) do
      add :client_data, :text
    end
  end
end
