defmodule WockyAPI.Callbacks.BotSubscription do
  @moduledoc """
  Callbacks for DB bot changes
  """

  use DawdleDB.Handler, type: Wocky.Bot.Subscription

  alias Wocky.Bot.Subscription
  alias Wocky.Repo
  alias WockyAPI.Resolvers.Bot, as: BotResolver

  def handle_update(
        %Subscription{visitor: b} = subscriber,
        %Subscription{visitor: a}
      )
      when a != b do
    subscriber = Repo.preload(subscriber, [:bot, :user])

    if subscriber.bot != nil && subscriber.user != nil do
      BotResolver.notify_visitor_subscription(
        subscriber.bot,
        subscriber.user,
        subscriber.visitor,
        subscriber.updated_at
      )
    end
  end

  def handle_update(_new, _old), do: :ok
end
