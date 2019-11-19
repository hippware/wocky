defmodule Wocky.Callbacks.Bot do
  @moduledoc """
  Callbacks for DB bot changes
  """

  use DawdleDB.Handler, type: Wocky.POI.Bot

  alias Wocky.Location
  alias Wocky.POI.Bot
  alias Wocky.Repo

  @impl true
  def handle_update(%Bot{location: new} = bot, %Bot{location: old})
      when new != old do
    bot_with_subs = Repo.preload(bot, [:subscribers])

    for user <- bot_with_subs.subscribers do
      Location.refresh_bot_subscriptions(user)
    end
  end

  def handle_update(_new, _old), do: :ok
end
