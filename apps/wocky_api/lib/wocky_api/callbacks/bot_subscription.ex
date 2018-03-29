defmodule WockyAPI.Callbacks.BotSubscription do
  @moduledoc """
  Callbacks for DB bot changes
  """
  alias Absinthe.Subscription, as: AbsintheSub
  alias Wocky.Bot.Subscription
  alias Wocky.Repo
  alias Wocky.Watcher.Client
  alias WockyAPI.Endpoint
  alias WockyDBWatcher.Event

  def register do
    Client.subscribe(Subscription, :update, &handle_update/1)
  end

  def handle_update(%Event{action: :update, old: old, new: new}) do
    if old.visitor != new.visitor do
      sub = Repo.preload(new, [:bot])
      if (sub.bot != nil) do
        AbsintheSub.publish(Endpoint, sub.bot, [bot_visitors: sub.bot.id])
      end
    end
  end
end
