defmodule Wocky.Watcher.Callbacks.BotSubscription do
  @moduledoc """
  Callbacks for DB bot subscription changes
  """
  alias Wocky.Bot.Subscription
  alias Wocky.Push
  alias Wocky.Push.Events.BotGeofenceShareEvent
  alias Wocky.Repo
  alias Wocky.Watcher.Client
  alias WockyDBWatcher.Event

  def register do
    Client.subscribe(Subscription, :insert, &handle_insert/1)
    Client.subscribe(Subscription, :update, &handle_update/1)
  end

  def handle_insert(%Event{
        action: :insert,
        new: %Subscription{guest: true} = sub
      }) do
    notify_owner_new_guest(sub)
  end

  def handle_insert(_), do: :ok

  def handle_update(%Event{
        action: :update,
        old: %Subscription{guest: false},
        new: %Subscription{guest: true} = sub
      }) do
    notify_owner_new_guest(sub)
  end

  def handle_update(_), do: :ok

  defp notify_owner_new_guest(sub) do
    sub = Repo.preload(sub, [:user, bot: [:user]])

    if sub.user != nil && sub.bot != nil && sub.bot.user != nil do
      # Don't send a notification about the owner to themself
      if sub.user.id != sub.bot.user.id do
        event =
          BotGeofenceShareEvent.new(%{
            from: sub.user,
            to: sub.bot.user,
            bot: sub.bot,
            type: :accept
          })

        Push.notify_all(sub.bot.user.id, event)
      end
    end
  end
end
