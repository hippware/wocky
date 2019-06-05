defmodule Wocky.Repo.Migrations.AddWelcomedField do
  use Wocky.Repo.Migration

  alias Wocky.Account.User
  alias Wocky.Repo

  def change do
    alter table(:users) do
      add :welcome_sent, :boolean, null: false, default: false
    end

    flush()

    Repo.update_all(User, set: [welcome_sent: true])
  end
end
