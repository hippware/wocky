defmodule Wocky.Callbacks.Block do
  @moduledoc """
  Callbacks for Block opertaions
  """

  use DawdleDB.Handler, type: Wocky.Block

  alias Wocky.Block
  alias Wocky.Bots
  alias Wocky.Notifier.InBand.Notification
  alias Wocky.Relations
  alias Wocky.Repo.Hydrator

  def handle_insert(new) do
    Hydrator.with_assocs(new, [:blocker, :blockee], fn %Block{
                                                         blocker: blocker,
                                                         blockee: blockee
                                                       } ->
      # Delete content items on owned bots by blocker/blockee
      delete_bot_references(blocker, blockee)
      delete_bot_references(blockee, blocker)

      # Delete invitations
      Relations.delete_invitation(blocker, blockee)
      Relations.delete_invitation(blockee, blocker)

      # Delete notifications
      Notification.delete(blocker, blockee)
      Notification.delete(blockee, blocker)

      :ok
    end)
  end

  defp delete_bot_references(a, b) do
    a
    |> Relations.get_owned_bots()
    |> Enum.each(fn bot ->
      Bots.delete_items(bot, b)
      Relations.unsubscribe(b, bot)
    end)
  end
end
