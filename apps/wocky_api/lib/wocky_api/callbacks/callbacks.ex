defmodule WockyAPI.Callbacks do
  @moduledoc """
  Initialisation for wocky DB watcher callbacks
  """

  @modules [
    WockyAPI.Callbacks.BotSubscription,
    WockyAPI.Callbacks.LocationChanged,
    WockyAPI.Callbacks.Message,
    WockyAPI.Callbacks.Notification,
    WockyAPI.Callbacks.Presence,
    WockyAPI.Callbacks.Relationship,
    WockyAPI.Callbacks.User
  ]

  def register do
    Enum.each(@modules, fn m -> m.register() end)
  end
end
