defmodule Wocky.Callbacks.BotItem do
  @moduledoc """
  Callbacks for DB bot item changes
  """

  use DawdleDB.Handler, type: Wocky.Bot.Item

  alias Wocky.Repo
  alias Wocky.User.Notification.BotItem

  def handle_insert(item) do
    item = Repo.preload(item, [:bot])

    if item.bot != nil do
      BotItem.notify(item)
    end
  end
end
