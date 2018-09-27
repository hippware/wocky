defmodule WockyXMPP.BotCallbacks do
  @moduledoc """
  Callbacks for DB bot changes
  """

  use Wocky.Watcher, type: Wocky.Bot, events: [:update]

  def handle_update(%Event{action: :update, old: old, new: new}) do
    :wocky_bot_users.maybe_notify_desc_change(old, new)
  end
end
