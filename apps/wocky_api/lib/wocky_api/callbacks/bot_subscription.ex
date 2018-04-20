defmodule WockyAPI.Callbacks.BotSubscription do
  @moduledoc """
  Callbacks for DB bot changes
  """
  alias Wocky.Bot.Subscription
  alias Wocky.Repo
  alias Wocky.Watcher.Client
  alias WockyAPI.Resolvers.Bot, as: BotResolver
  alias WockyDBWatcher.Event

  def register do
    Client.subscribe(Subscription, :update, &handle_update/1)
  end

  def handle_update(
    %Event{action: :update,
      old: %Subscription{visitor: a},
      new: %Subscription{visitor: b} = subscriber})
  when a != b do
    subscriber = Repo.preload(subscriber, [:bot, :user])
    BotResolver.notify_visitor_subscription(
      subscriber.bot, subscriber.user, subscriber.visitor)
  end

  def handle_update(_), do: :ok
end
