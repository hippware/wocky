defmodule Wocky.Repo.Migrations.AddBotCreatedFlag do
  use Ecto.Migration

  def up do
    alter table(:users) do
      add :bot_created, :boolean, default: false, null: false
    end

    flush()

    execute """
    UPDATE users SET bot_created = true WHERE (SELECT count(*) FROM bots
      WHERE user_id = users.id) > 0;
    """
  end

  def down do
    alter table(:users) do
      remove :bot_created
    end
  end
end
