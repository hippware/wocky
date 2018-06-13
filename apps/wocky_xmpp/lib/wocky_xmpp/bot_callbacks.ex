defmodule WockyXMPP.BotCallbacks do
  @moduledoc """
  Callbacks for DB bot changes
  """
  alias Wocky.Bot
  alias Wocky.Watcher.Client
  alias WockyDBWatcher.Event

  def register do
    Client.subscribe(Bot, :insert, &handle_insert/1)
    Client.subscribe(Bot, :update, &handle_update/1)
  end

  def handle_insert(%Event{action: :insert, new: new}) do
    :wocky_bot_users.notify_new_viewers(new, :none, new.public)
  end

  def handle_update(%Event{action: :update, old: old, new: new}) do
    :wocky_bot_users.notify_new_viewers(new, old.public, new.public)
    :wocky_bot_users.maybe_notify_desc_change(old, new)
  end
end
