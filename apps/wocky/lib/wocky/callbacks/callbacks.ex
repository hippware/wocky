defmodule Wocky.Callbacks do
  @moduledoc """
  Initialisation for wocky DB watcher callbacks
  """

  alias Wocky.Callbacks.Bot
  alias Wocky.Callbacks.BotInvitation
  alias Wocky.Callbacks.BotItem
  alias Wocky.Callbacks.BotSubscription
  alias Wocky.Callbacks.LocationShare
  alias Wocky.Callbacks.Message
  alias Wocky.Callbacks.Presence
  alias Wocky.Callbacks.RosterInvitation
  alias Wocky.Callbacks.TROSMetadata

  @modules [
    Bot,
    BotInvitation,
    BotItem,
    BotSubscription,
    LocationShare,
    Message,
    Presence,
    RosterInvitation,
    TROSMetadata
  ]

  def register do
    Enum.each(@modules, fn m -> m.register() end)
  end
end
