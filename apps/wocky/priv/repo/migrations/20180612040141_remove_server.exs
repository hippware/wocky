defmodule Wocky.Repo.Migrations.RemoveServer do
  use Ecto.Migration

  def change do
    alter table(:users), do: remove :server
    alter table(:bots), do: remove :server
  end
end
