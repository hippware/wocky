defmodule Wocky.Repo.Migrations.AddMessageClientData do
  use Wocky.Repo.Migration

  def change do
    alter table(:messages) do
      add :client_data, :text
    end
  end
end
