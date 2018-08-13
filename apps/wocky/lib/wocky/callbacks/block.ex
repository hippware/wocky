defmodule Wocky.Callbacks.Block do
  @moduledoc """
  Callbacks for Block opertaions
  """

  use Wocky.Watcher, type: Wocky.Block, events: [:insert]

  alias Wocky.Block
  alias Wocky.Bot
  alias Wocky.Bot.Item
  alias Wocky.Bot.Share
  alias Wocky.Conversation
  alias Wocky.HomeStream
  alias Wocky.Repo
  alias Wocky.User

  def handle_insert(%Event{action: :insert, new: new}) do
    %Block{blocker: blocker, blockee: blockee} =
      Repo.preload(new, [:blocker, :blockee])

    if blocker != nil && blockee != nil do
      # Delete HS message items
      HomeStream.delete(blocker, blockee)
      HomeStream.delete(blockee, blocker)

      # Delete HS bot items for bots owned by blocked user
      # and bot shares of blocked user's bots
      # and content items on owned bots by blocker/blockee
      delete_bot_references(blocker, blockee)
      delete_bot_references(blockee, blocker)

      # Delete conversations and MAM entries between the users
      delete_message_logs(blocker, blockee)
      delete_message_logs(blockee, blocker)
      :ok
    end
  end

  defp delete_bot_references(a, b) do
    a
    |> User.get_owned_bots()
    |> Enum.each(fn bot ->
      HomeStream.delete(b, bot)
      Item.delete(bot, b)
      Share.delete(b, bot)
      Bot.unsubscribe(bot, b)
    end)
  end

  defp delete_message_logs(a, b) do
    Conversation.delete_user_pair(a, b)
  end
end
