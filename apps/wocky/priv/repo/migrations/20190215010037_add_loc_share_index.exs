defmodule Wocky.Repo.Migrations.AddLocShareIndex do
  use Wocky.Repo.Migration

  def change do
    create index(:user_location_shares, [:expires_at])
  end
end
