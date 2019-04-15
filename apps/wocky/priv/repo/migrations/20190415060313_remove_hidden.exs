defmodule Wocky.Repo.Migrations.RemoveHidden do
  use Wocky.Repo.Migration

  def change do
    alter table(:users), do: remove :hidden_until
  end
end
