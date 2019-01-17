defmodule Wocky.Repo.Migrations.AddSmsCount do
  use Ecto.Migration

  def change do
    alter table(:users) do
      add :smss_sent, :integer, default: 0, null: false
    end
  end
end
