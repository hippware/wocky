defmodule Wocky.Callbacks.Block do
  @moduledoc """
  Callbacks for Block opertaions
  """

  use DawdleDB.Handler, type: Wocky.Block

  alias Wocky.Block
  alias Wocky.Location
  alias Wocky.Notifier.InBand.Notification
  alias Wocky.POI
  alias Wocky.Relation
  alias Wocky.Repo.Hydrator

  def handle_insert(new) do
    Hydrator.with_assocs(new, [:blocker, :blockee], fn %Block{
                                                         blocker: blocker,
                                                         blockee: blockee
                                                       } ->
      Enum.each(
        [{blocker, blockee}, {blockee, blocker}],
        fn {a, b} ->
          # Delete content items on owned bots by blocker/blockee
          delete_bot_references(a, b)

          # Delete invitations
          Relation.delete_invitation(a, b)

          # Delete notifications
          Notification.delete(a, b)

          # Delete live location shares
          Location.stop_sharing_location(a, b)
        end
      )

      :ok
    end)
  end

  defp delete_bot_references(a, b) do
    a
    |> Relation.get_owned_bots()
    |> Enum.each(fn bot ->
      POI.delete_items(bot, b)
      Relation.unsubscribe(b, bot)
    end)
  end
end
