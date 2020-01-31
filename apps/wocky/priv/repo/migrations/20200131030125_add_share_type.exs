defmodule Wocky.Repo.Migrations.AddShareType do
  use Wocky.Repo.Migration

  alias Wocky.Contacts.Relationship.LocationShareTypeEnum

  def change do
    alter table("notifications") do
      add :share_type, LocationShareTypeEnum.type(), default: nil
    end
  end
end
