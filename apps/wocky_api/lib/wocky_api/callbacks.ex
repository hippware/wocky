defmodule WockyAPI.Callbacks do
  @moduledoc """
  Initialisation for wocky DB watcher callbacks
  """

  alias WockyAPI.Callbacks.{BotSubscription, Message, Notification, RosterItem}

  @modules [BotSubscription, Message, Notification, RosterItem]

  def register do
    Enum.each(@modules, fn m -> m.register() end)
  end
end
