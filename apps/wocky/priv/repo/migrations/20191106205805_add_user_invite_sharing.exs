defmodule Wocky.Repo.Migrations.AddUserInviteSharing do
  use Wocky.Repo.Migration

  alias Wocky.Contacts.Relationship.LocationShareTypeEnum

  def change do
    alter table(:user_invite_codes) do
      add :phone_number, :string

      add :share_type, LocationShareTypeEnum.type(),
        default: "disabled",
        null: false
    end
  end
end
