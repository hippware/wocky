defmodule Wocky.Watcher.Callbacks do
  @moduledoc """
  Initialisation for wocky DB watcher callbacks
  """

  alias Wocky.Watcher.Callbacks.Bot

  def register do
    Bot.register()
  end
end
