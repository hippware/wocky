defmodule Wocky.Repo.Migrations.RemoveIsFetch do
  use Ecto.Migration

  def change do
    alter table(:user_locations) do
      remove :is_fetch, :boolean, default: false
    end

    alter table(:user_current_location) do
      remove :is_fetch, :boolean, default: false
    end
  end
end
