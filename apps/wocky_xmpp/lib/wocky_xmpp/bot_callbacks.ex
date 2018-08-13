defmodule WockyXMPP.BotCallbacks do
  @moduledoc """
  Callbacks for DB bot changes
  """

  use Wocky.Watcher, type: Wocky.Bot, events: [:insert, :update]

  def handle_insert(%Event{action: :insert, new: new}) do
    :wocky_bot_users.notify_new_viewers(new, :none, new.public)
  end

  def handle_update(%Event{action: :update, old: old, new: new}) do
    :wocky_bot_users.notify_new_viewers(new, old.public, new.public)
    :wocky_bot_users.maybe_notify_desc_change(old, new)
  end
end
