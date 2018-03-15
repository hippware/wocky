defmodule Wocky.Repo.Migrations.RemoveFollowMe do
  use Ecto.Migration

  def change do
    alter table(:bots) do
      remove :follow_me
      remove :follow_me_expiry
    end
  end
end
