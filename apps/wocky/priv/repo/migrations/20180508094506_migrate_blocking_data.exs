defmodule Wocky.Repo.Migrations.MigrateBlockingData do
  use Wocky.Repo.Migration

  import Ecto.Query

  alias Wocky.Block
  alias Wocky.Repo
  alias Wocky.Roster
  alias Wocky.Roster.Item

  def up do
    Item
    |> Repo.stream()
    |> Stream.each(&migrate_blocking/1)
    |> Stream.run()
  end

  defp migrate_blocking(%Item{groups: groups} = item) do
    if Enum.member?(groups, Roster.blocked_group()) do
      i = Repo.preload(item, [:user, :contact])
      Block.block(i.user, i.contact)
    end
    # The new `put/1` will strip the blocking groups out
    item
    |> Map.from_struct()
    |> Roster.put()
  end
end
