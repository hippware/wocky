defmodule Wocky.Callbacks do
  @moduledoc """
  Initialisation for wocky DB watcher callbacks
  """

  alias Wocky.Callbacks.Block
  alias Wocky.Callbacks.Bot
  alias Wocky.Callbacks.BotInvitation
  alias Wocky.Callbacks.BotItem
  alias Wocky.Callbacks.RosterItem
  alias Wocky.Callbacks.User

  @modules [
    Block,
    Bot,
    BotInvitation,
    BotItem,
    RosterItem,
    User
  ]

  def register do
    Enum.each(@modules, fn m -> m.register() end)
  end
end
