defmodule Wocky.Repo.Migrations.ExpandInitialContacts do
  use Wocky.Repo.Migration

  def change do
    rename table(:initial_followees), to: table(:initial_contacts)

    alter table(:initial_contacts) do
      add :type, :string, null: false, default: "followee"
    end
  end
end
