defmodule WockyAPI.Callbacks do
  @moduledoc """
  Initialisation for wocky DB watcher callbacks
  """

  alias WockyAPI.Callbacks.{BotSubscription, HomeStreamItem, Notification, RosterItem}

  @modules [BotSubscription, HomeStreamItem, Notification, RosterItem]

  def register do
    Enum.each(@modules, fn m -> m.register() end)
  end
end
