defmodule Wocky.Repo.Migrations.HandleOnlySearch do
  use Wocky.Repo.Migration

  def up do
    execute """
    UPDATE users SET name = '' WHERE name IS NULL;
    """

    alter table(:users) do
      modify :name, :string, null: false, default: ""
    end
  end
end
