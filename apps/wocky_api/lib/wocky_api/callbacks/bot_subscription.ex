defmodule WockyAPI.Callbacks.BotSubscription do
  @moduledoc """
  Callbacks for DB bot changes
  """
  alias Absinthe.Subscription, as: AbsintheSub
  alias Wocky.Bot.Subscription
  alias Wocky.Watcher.Client
  alias WockyAPI.Endpoint
  alias WockyDBWatcher.Event

  def register do
    Client.subscribe(Subscription, :update, &handle_update/1)
  end

  def handle_update(%Event{action: :update, old: old, new: new}) do
    if old.visit != new.visit do
      AbsintheSub.publish(Endpoint, new, [bot_visitors: new.id])
    end
  end
end
