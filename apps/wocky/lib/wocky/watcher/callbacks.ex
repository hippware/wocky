defmodule Wocky.Watcher.Callbacks do
  @moduledoc """
  Initialisation for wocky DB watcher callbacks
  """

  alias Wocky.Watcher.Callbacks.Bot
  alias Wocky.Watcher.Callbacks.BotItem

  @modules [Bot, BotItem]

  def register do
    Enum.each(@modules, fn m -> m.register() end)
  end
end
