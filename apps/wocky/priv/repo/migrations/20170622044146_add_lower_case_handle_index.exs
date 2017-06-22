defmodule Wocky.Repo.Migrations.AddLowerCaseHandleIndex do
  use Ecto.Migration

  def change do
    drop unique_index(:users, [:handle])
    create unique_index(:users, ["lower(handle)"], name: :users_lower_handle_index)
  end
end
