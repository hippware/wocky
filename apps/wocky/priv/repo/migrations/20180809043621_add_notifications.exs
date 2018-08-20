defmodule Wocky.Repo.Migrations.AddNotifications do
  use Wocky.Repo.Migration

  alias Wocky.Repo.Migration.Utils

  def change do
    create table(:notifications) do
      add :user_id,         references(:users, type: :uuid, on_delete: :delete_all), null: false

      add :type, :string, null: false

      add :other_user_id,  references(:users, type: :uuid, on_delete: :delete_all), null: false
      add :bot_id,         references(:bots, type: :uuid, on_delete: :delete_all)
      add :bot_item_id,    references(:bot_items, type: :uuid, on_delete: :delete_all)
      add :invitation_id,  references(:bot_invitations, on_delete: :delete_all)
      add :geofence_event, :string
      add :invitation_accepted, :boolean

      timestamps()
    end

    flush()

    Utils.update_notify("notifications", [:insert])
  end
end
