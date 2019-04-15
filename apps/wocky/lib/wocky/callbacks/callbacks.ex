defmodule Wocky.Callbacks do
  @moduledoc """
  Initialisation for wocky DB watcher callbacks
  """

  alias Wocky.Callbacks.{
    Block,
    Bot,
    BotInvitation,
    BotItem,
    LocationShare,
    Message,
    RosterItem,
    TROSMetadata,
    UserInvitation
  }

  @modules [
    Block,
    Bot,
    BotInvitation,
    BotItem,
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
