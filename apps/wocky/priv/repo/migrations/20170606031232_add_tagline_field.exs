defmodule Wocky.Repo.Migrations.AddTaglineField do
  use Ecto.Migration

  def change do
    alter table(:users) do
      add :tagline, :text, null: false, default: ""
    end
  end
end
