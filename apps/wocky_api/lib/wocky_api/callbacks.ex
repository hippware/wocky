defmodule WockyAPI.Callbacks do
  @moduledoc """
  Initialisation for wocky DB watcher callbacks
  """

  alias WockyAPI.Callbacks.BotSubscription
  alias WockyAPI.Callbacks.LocationChanged
  alias WockyAPI.Callbacks.Message
  alias WockyAPI.Callbacks.Notification
  alias WockyAPI.Callbacks.Presence
  alias WockyAPI.Callbacks.RosterItem
  alias WockyAPI.Callbacks.User

  @modules [
    BotSubscription,
    LocationChanged,
    Message,
    Notification,
    Presence,
    RosterItem,
    User
  ]

  def register do
    Enum.each(@modules, fn m -> m.register() end)
  end
end
