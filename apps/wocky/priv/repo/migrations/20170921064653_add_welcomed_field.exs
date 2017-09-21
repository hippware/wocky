defmodule Wocky.Repo.Migrations.AddWelcomedField do
  use Wocky.Repo.Migration
  use Wocky.Repo.Model

  alias Wocky.Repo
  alias Wocky.User

  def change do
    alter table(:users) do
      add :welcome_sent, :boolean, null: false, default: false
    end

    flush()

    Repo.update_all(User, set: [welcome_sent: true])
  end
end
