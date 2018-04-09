defmodule WockyAPI.Callbacks.Bot do
  @moduledoc """
  Callbacks for DB bot changes
  """
  alias Absinthe.Subscription
  alias Wocky.Bot
  alias Wocky.Watcher.Client
  alias WockyAPI.Endpoint
  alias WockyDBWatcher.Event

  def register do
    Client.subscribe(Bot, :update, &handle_update/1)
  end

  def handle_update(%Event{action: :update, old: old, new: new}) do
    if old.visitors_count != new.visitors_count do
      Subscription.publish(Endpoint, new, [bot_visitors: new.id])
    end
  end
end
