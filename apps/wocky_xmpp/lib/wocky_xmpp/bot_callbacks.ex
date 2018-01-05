defmodule WockyXMPP.BotCallbacks do
  use GenStage

  alias Wocky.Bot
  alias WockyDBWatcher.Watcher

  def start_link, do: GenStage.start_link(__MODULE__, nil, name: __MODULE__)

  def init(_) do
    watchers =
    Enum.map([:insert, :update],
             fn(a) ->
               {:ok, w} = WockyDBWatcher.start_watcher(Bot, a, "bot_callbacks")
               w
             end)
    {:consumer, nil, subscribe_to: watchers}
  end

  def handle_events(events, _from, state) do
    Enum.each(events, &handle_event/1)
    {:noreply, [], state}
  end

  defp handle_event(%Watcher{action: :insert, new: new}) do
    :wocky_bot_users.notify_new_viewers(new.server, new, :none, new.public)
  end

  defp handle_event(%Watcher{action: :update, old: old, new: new}) do
    :wocky_bot_users.notify_new_viewers(new.server, new, old.public, new.public)
    :wocky_bot_users.maybe_update_hs_items(old, new)
    :wocky_bot_users.maybe_notify_desc_change(old, new)
  end

  #wocky_bot_users:send_notification/4 trigger (bot share created)
end
