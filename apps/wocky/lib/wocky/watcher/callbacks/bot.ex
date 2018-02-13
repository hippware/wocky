defmodule Wocky.Watcher.Callbacks.Bot do
  @moduledoc """
  Callbacks for DB bot changes
  """
  alias Wocky.Bot
  alias Wocky.Watcher.Client
  alias WockyDBWatcher.Event

  def register do
    Client.subscribe(Bot, :update, &handle_update/1)
  end

  def handle_update(%Event{action: :update, old: old, new: new}) do
    Bot.maybe_update_hs_items(old, new)
  end
end
