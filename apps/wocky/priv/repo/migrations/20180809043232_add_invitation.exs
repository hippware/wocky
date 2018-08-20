# credo:disable-for-this-file Credo.Check.Readability.MaxLineLength
defmodule Wocky.Repo.Migrations.AddInvitation do
  use Wocky.Repo.Migration

  alias Wocky.Repo.Migration.Utils

  def change do
    create table(:bot_invitations) do
      add :user_id,         references(:users, type: :uuid, on_delete: :delete_all), null: false
      add :invitee_id,      references(:users, type: :uuid, on_delete: :delete_all), null: false
      add :bot_id,          references(:bots, type: :uuid, on_delete: :delete_all), null: false
      add :accepted,        :boolean

      timestamps()
    end

    create unique_index("bot_invitations", [:user_id, :bot_id, :invitee_id])

    Utils.update_notify("bot_invitations", [:insert, :update])
  end
end
