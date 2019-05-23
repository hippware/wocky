defmodule Wocky.Callbacks.BotSubscription do
  @moduledoc """
  Callbacks for DB bot changes
  """

  use DawdleDB.Handler, type: Wocky.Bot.Subscription

  alias Wocky.Bot
  alias Wocky.Repo
  alias Wocky.User.Location.Handler, as: LocationHandler

  def handle_delete(subscription) do
    subscription = Repo.preload(subscription, [:user])

    if subscription.user != nil do
      LocationHandler.remove_subscription(
        subscription.user,
        %Bot{id: subscription.bot_id}
      )
    end
  end
end
