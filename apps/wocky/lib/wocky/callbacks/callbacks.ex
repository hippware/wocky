defmodule Wocky.Callbacks do
  @moduledoc """
  Initialisation for wocky DB watcher callbacks
  """

  alias Wocky.Callbacks.Block
  alias Wocky.Callbacks.Bot
  alias Wocky.Callbacks.BotInvitation
  alias Wocky.Callbacks.BotItem
  alias Wocky.Callbacks.BotSubscription
  alias Wocky.Callbacks.LocationShare
  alias Wocky.Callbacks.Message
  alias Wocky.Callbacks.RosterItem
  alias Wocky.Callbacks.TROSMetadata
  alias Wocky.Callbacks.UserInvitation

  @modules [
    Block,
    Bot,
    BotInvitation,
    BotItem,
    BotSubscription,
    LocationShare,
    Message,
    RosterItem,
    TROSMetadata,
    UserInvitation
  ]

  def register do
    Enum.each(@modules, fn m -> m.register() end)
  end
end
