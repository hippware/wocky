defmodule Wocky.Repo.Migrations.SetBotRadius do
  use Wocky.Repo.Migration

  alias Wocky.Bot
  alias Wocky.Repo

  def up do
    alter table(:bots) do
      modify :radius, :float, null: false, default: 100.0
    end

    flush()

    Repo.update_all(Bot, set: [radius: 100.0])
  end

  def down do
    alter table(:bots) do
      modify :radius, :float, null: false, default: 0.0
    end
  end
end
