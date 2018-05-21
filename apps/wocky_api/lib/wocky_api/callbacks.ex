defmodule WockyAPI.Callbacks do
  @moduledoc """
  Initialisation for wocky DB watcher callbacks
  """

  alias WockyAPI.Callbacks.BotSubscription
  alias WockyAPI.Callbacks.HomeStreamItem

  @modules [BotSubscription, HomeStreamItem]

  def register do
    Enum.each(@modules, fn m -> m.register() end)
  end
end
