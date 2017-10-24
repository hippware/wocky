defmodule Wocky.Repo.Migrations.PushTokenFlags do
  use Ecto.Migration

  def change do
    alter table(:devices) do
      add :invalid, :boolean, null: false, default: false
      add :feedback, :boolean, null: false, default: false
    end

    create index(:devices, [:token])
  end
end
