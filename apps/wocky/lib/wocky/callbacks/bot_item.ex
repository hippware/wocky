defmodule Wocky.Callbacks.BotItem do
  @moduledoc """
  Callbacks for DB bot item changes
  """

  use Wocky.Watcher, type: Wocky.Bot.Item, events: [:insert]

  alias Wocky.Repo
  alias Wocky.User.Notification.BotItem

  def handle_insert(%Event{new: item}) do
    item = Repo.preload(item, [:bot])

    if item.bot != nil do
      BotItem.notify(item)
    end
  end
end
