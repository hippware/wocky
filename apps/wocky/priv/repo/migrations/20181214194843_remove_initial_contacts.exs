defmodule Wocky.Repo.Migrations.RemoveInitialContacts do
  use Ecto.Migration

  def up do
    drop table(:initial_contacts)
  end
end
