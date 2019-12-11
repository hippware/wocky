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
    Wocky.Callbacks.Message,
    Wocky.Callbacks.Relationship,
    Wocky.Callbacks.TROSMetadata,
    Wocky.Callbacks.UserProximity
  ]

  @spec register :: :ok
  def register do
    Enum.each(@modules, fn m -> m.register() end)
  end
end
