defmodule Wocky.Callbacks.BotSubscription do
  @moduledoc """
  Callbacks for DB bot changes
  """

  use DawdleDB.Handler, type: Wocky.Bots.Subscription

  alias Wocky.Bots.Bot
  alias Wocky.Location
  alias Wocky.Repo.Hydrator

  def handle_delete(new) do
    Hydrator.with_assocs(new, [:user], fn rec ->
      Location.remove_subscription(
        rec.user,
        %Bot{id: rec.bot_id}
      )
    end)
  end
end
