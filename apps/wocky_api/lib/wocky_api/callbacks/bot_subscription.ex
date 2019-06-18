defmodule WockyAPI.Callbacks.BotSubscription do
  @moduledoc """
  Callbacks for DB bot changes
  """

  use DawdleDB.Handler, type: Wocky.Relations.Subscription

  alias Wocky.Relations.Subscription
  alias Wocky.Repo.Hydrator
  alias WockyAPI.Resolvers.Bot, as: BotResolver

  def handle_update(
        %Subscription{visitor: b} = new,
        %Subscription{visitor: a}
      )
      when a != b do
    Hydrator.with_assocs(new, [:bot, :user], fn rec ->
      BotResolver.notify_visitor_subscription(
        rec.bot,
        rec.user,
        rec.visitor,
        rec.updated_at
      )
    end)
  end

  def handle_update(_new, _old), do: :ok
end
