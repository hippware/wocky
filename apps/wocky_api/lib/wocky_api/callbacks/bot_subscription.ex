defmodule WockyAPI.Callbacks.BotSubscription do
  @moduledoc """
  Callbacks for DB bot changes
  """

  use Wocky.Watcher, type: Wocky.Bot.Subscription, events: [:update]

  alias Wocky.Bot.Subscription
  alias Wocky.Repo
  alias WockyAPI.Resolvers.Bot, as: BotResolver

  def handle_update(%Event{
        action: :update,
        old: %Subscription{visitor: a},
        new: %Subscription{visitor: b} = subscriber
      })
      when a != b do

    subscriber = Repo.preload(subscriber, [:bot, :user])

    if subscriber.bot != nil && subscriber.user != nil do
      BotResolver.notify_visitor_subscription(
        subscriber.bot,
        subscriber.user,
        subscriber.visitor
      )
    end
  end

  def handle_update(_), do: :ok
end
