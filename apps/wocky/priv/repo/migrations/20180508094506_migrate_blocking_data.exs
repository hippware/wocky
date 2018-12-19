defmodule Wocky.Repo.Migrations.MigrateBlockingData do
  use Wocky.Repo.Migration

  # alias Wocky.Block
  # alias Wocky.Repo
  # alias Wocky.Roster
  # alias Wocky.Roster.Item

  # This no longer works due to changes in the Item structures. We can just
  # take it all out, though, because it's only for migrating old data (rather
  # than making schema changes) and there shouldn't be any data that old in
  # existence anywhere.

  def up do
  #   Item
  #   |> Repo.stream()
  #   |> Stream.each(&migrate_blocking/1)
  #   |> Stream.run()
    :ok
  end

  # defp migrate_blocking(%Item{groups: groups} = item) do
  #   if Enum.member?(groups, Roster.blocked_group()) do
  #     i = Repo.preload(item, [:user, :contact])
  #     Block.block(i.user, i.contact)
  #   end
  #   # The new `put/1` will strip the blocking groups out
  #   item
  #   |> Map.from_struct()
  #   |> Roster.put()
  # end
end
