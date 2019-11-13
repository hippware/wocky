defmodule Wocky.Repo.Migrations.AddUserInviteSharing do
  use Ecto.Migration

  alias Wocky.Friends.Friend.LocationShareTypeEnum

  def change do
    alter table(:user_invite_codes) do
      add :phone_number, :string

      add :share_type, LocationShareTypeEnum.type(),
        default: "disabled",
        null: false
    end
  end
end
