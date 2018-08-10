defmodule Wocky.Callbacks do
  @moduledoc """
  Initialisation for wocky DB watcher callbacks
  """

  alias Wocky.Callbacks.Block
  alias Wocky.Callbacks.Bot
  alias Wocky.Callbacks.BotItem
  alias Wocky.Callbacks.BotSubscription
  alias Wocky.Callbacks.User

  @modules [Block, Bot, BotItem, BotSubscription, User]

  def register do
    Enum.each(@modules, fn m -> m.register() end)
  end
end
