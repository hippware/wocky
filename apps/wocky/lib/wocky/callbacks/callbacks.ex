defmodule Wocky.Callbacks do
  @moduledoc """
  Initialisation for wocky DB watcher callbacks
  """

  alias Wocky.Callbacks.{
    Block,
    Bot,
    BotInvitation,
    BotItem,
    Message,
    User,
    UserInvitation
  }

  @modules [
    Block,
    Bot,
    BotInvitation,
    BotItem,
    Message,
    User,
    UserInvitation
  ]

  def register do
    Enum.each(@modules, fn m -> m.register() end)
  end
end
