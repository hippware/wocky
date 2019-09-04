defmodule Wocky.Callbacks.BotSubscription do
  @moduledoc """
  Callbacks for DB bot changes
  """

  use DawdleDB.Handler, type: Wocky.Relation.Subscription

  alias Wocky.Location
  alias Wocky.POI
  alias Wocky.Repo.Hydrator
  alias Wocky.Waiter

  def handle_insert(new) do
    Hydrator.with_assocs(new, [:user, :bot], fn rec ->
      if rec.user_id == rec.bot.user_id do
        rec.bot
        |> POI.sub_setup_event()
        |> Waiter.notify()
      end

      Location.refresh_bot_subscriptions(rec.user)
    end)
  end

  def handle_delete(old) do
    Hydrator.with_assocs(old, [:user], fn rec ->
      Location.refresh_bot_subscriptions(rec.user)
    end)
  end
end
