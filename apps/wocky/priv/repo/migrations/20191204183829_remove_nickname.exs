defmodule Wocky.Repo.Migrations.RemoveNickname do
  use Wocky.Repo.Migration

  def change do
    alter table("roster_items") do
      remove :name, :string, null: false
    end
  end
end
