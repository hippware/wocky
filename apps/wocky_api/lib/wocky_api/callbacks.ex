defmodule WockyAPI.Callbacks do
  @moduledoc """
  Initialisation for wocky DB watcher callbacks
  """

  alias WockyAPI.Callbacks.{
    BotSubscription,
    CurrentLocation,
    Message,
    Notification,
    RosterItem,
    User
  }

  @modules [
    BotSubscription,
    CurrentLocation,
    Message,
    Notification,
    RosterItem,
    User
  ]

  def register do
    Enum.each(@modules, fn m -> m.register() end)
  end
end
