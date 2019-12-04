defmodule Wocky.Callbacks do
  @moduledoc """
  Initialisation for wocky DB watcher callbacks
  """

  @modules [
    Wocky.Callbacks.Bot,
    Wocky.Callbacks.BotInvitation,
    Wocky.Callbacks.BotItem,
    Wocky.Callbacks.BotSubscription,
    Wocky.Callbacks.Connection,
    Wocky.Callbacks.Friend,
    Wocky.Callbacks.FriendInvitation,
    Wocky.Callbacks.Message,
    Wocky.Callbacks.TROSMetadata
  ]

  @spec register :: :ok
  def register do
    Enum.each(@modules, fn m -> m.register() end)
  end
end
