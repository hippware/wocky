defmodule WockyAPI.Callbacks do
  @moduledoc """
  Initialisation for wocky DB watcher callbacks
  """

  alias WockyAPI.Callbacks.{Bot, BotSubscription, HomeStreamItem}

  @modules [Bot, BotSubscription, HomeStreamItem]

  def register do
    Enum.each(@modules, fn m -> m.register() end)
  end
end
