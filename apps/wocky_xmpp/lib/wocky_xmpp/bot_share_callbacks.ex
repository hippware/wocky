defmodule WockyXMPP.BotShareCallbacks do
  use GenStage

  alias Wocky.Bot.Share
  alias Wocky.Repo
  alias WockyDBWatcher.Watcher

  def start_link, do: GenStage.start_link(__MODULE__, nil, name: __MODULE__)

  def init(_) do
    {:ok, watcher} = WockyDBWatcher.start_watcher(
                      Share, :insert, "bot_share_callbacks")
    {:consumer, nil, subscribe_to: [watcher]}
  end

  def handle_events([], _from, state), do: {:noreply, [], state}
  def handle_events(
    [%Watcher{action: :insert, new: new} | tail], from, state) do
      share = Repo.preload(new, [:user, :sharer, :bot])
      :wocky_bot_users.send_share_notification(
                      share.sharer, share.user, share.bot)
      handle_events(tail, from, state)
  end
end
