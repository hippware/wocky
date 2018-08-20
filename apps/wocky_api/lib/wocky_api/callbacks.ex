defmodule WockyAPI.Callbacks do
  @moduledoc """
  Initialisation for wocky DB watcher callbacks
  """

  alias WockyAPI.Callbacks.{Bot, BotSubscription, HomeStreamItem, Notification}

  @modules [Bot, BotSubscription, HomeStreamItem, Notification]

  def register do
    Enum.each(@modules, fn m -> m.register() end)
  end
end
