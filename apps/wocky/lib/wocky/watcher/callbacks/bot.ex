defmodule Wocky.Watcher.Callbacks.Bot do
  @moduledoc """
  Callbacks for DB bot changes
  """
  alias Wocky.Bot
  alias Wocky.Repo
  alias Wocky.Watcher.Client
  alias WockyDBWatcher.Event

  def register do
    Client.subscribe(Bot, :insert, &handle_insert/1)
    Client.subscribe(Bot, :update, &handle_update/1)
  end

  def handle_insert(%Event{action: :insert, new: new}) do
    update_owner_subscription(new)
    Absinthe.Subscription.publish(WockyAPI.Endpoint, new, [bot_added: "bot_added"])
  end

  def handle_update(%Event{action: :update, old: old, new: new}) do
    Bot.maybe_update_hs_items(old, new)
    update_guests(old, new)
  end

  defp update_guests(%Bot{geofence: false}, %Bot{geofence: true} = bot) do
    update_owner_subscription(bot)
  end

  defp update_guests(%Bot{geofence: true}, %Bot{geofence: false} = bot) do
    Bot.clear_guests(bot)
  end

  defp update_guests(_, _), do: :ok

  defp update_owner_subscription(bot) do
    %{user: user} = Repo.preload(bot, [:user])

    if user != nil do
      Bot.subscribe(bot, user, bot.geofence)
    end
  end
end
