defmodule Wocky.Watcher.Callbacks do
  @moduledoc """
  Initialisation for wocky DB watcher callbacks
  """

  alias Wocky.Watcher.Callbacks.Block
  alias Wocky.Watcher.Callbacks.Bot
  alias Wocky.Watcher.Callbacks.BotItem
  alias Wocky.Watcher.Callbacks.BotSubscription
  alias Wocky.Watcher.Callbacks.User

  @modules [Block, Bot, BotItem, BotSubscription, User]

  def register do
    Enum.each(@modules, fn m -> m.register() end)
  end
end
