defmodule Wocky.Callbacks.Block do
  @moduledoc """
  Callbacks for Block opertaions
  """

  use DawdleDB.Handler, type: Wocky.Block

  alias Wocky.Account
  alias Wocky.Block
  alias Wocky.Bot
  alias Wocky.Bot.Invitation
  alias Wocky.Bot.Item
  alias Wocky.Notifier.InBand.Notification
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
      Invitation.delete(blocker, blockee)
      Invitation.delete(blockee, blocker)

      # Delete notifications
      Notification.delete(blocker, blockee)
      Notification.delete(blockee, blocker)

      :ok
    end)
  end

  defp delete_bot_references(a, b) do
    a
    |> Account.get_owned_bots()
    |> Enum.each(fn bot ->
      Item.delete(bot, b)
      Bot.unsubscribe(bot, b)
    end)
  end
end
