defmodule Wocky.Repo.Migrations.MergeSharingRoster do
  use Wocky.Repo.Migration

  import Ecto.Query

  alias Wocky.Repo
  alias Wocky.Roster.Item
  alias Wocky.Roster.Item.LocationShareTypeEnum

  defmodule OldShare do
    @moduledoc false

    use Wocky.Repo.Schema

    schema "user_location_shares" do
      field :user_id, :binary_id
      field :shared_with_id, :binary_id
    end
  end

  def up do
    LocationShareTypeEnum.create_type()

    alter table(:roster_items) do
      add :share_migrated, :boolean, null: false, default: true

      add :share_type, LocationShareTypeEnum.type(),
        default: "disabled",
        null: false
    end

    flush()

    Repo.update_all(Item, set: [share_migrated: false])

    # This isn't the most efficient way to do this, but there are currently
    # less than 100 rows to migrate in the worst case. Given that, I went for
    # the easy route.
    OldShare
    |> Repo.all()
    |> Enum.each(fn %{user_id: uid, shared_with_id: swid} ->
      Item
      |> where([i], i.user_id == ^uid and i.contact_id == ^swid)
      |> Repo.update_all(set: [share_type: "always", share_migrated: true])
    end)

    # I intentionally didn't delete the 'user_location_shares' table. There is
    # a short interval where the old code will be running on this schema, and
    # leaving the table in place will prevent hiccups. We can clean it up later.
  end

  def down do
    alter table(:roster_items) do
      remove :share_migrated
      remove :share_type
    end

    LocationShareTypeEnum.drop_type()
  end
end
