defmodule Wocky.Callbacks.Bot do
  @moduledoc """
  Callbacks for DB bot changes
  """

  use DawdleDB.Handler, type: Wocky.POI.Bot

  import Ecto.Query

  alias Wocky.Account.User
  alias Wocky.Location
  alias Wocky.POI.Bot
  alias Wocky.Repo

  @impl true
  def handle_update(%Bot{location: new} = bot, %Bot{location: old})
      when new != old do
    bot_with_sub_ids =
      Repo.preload(bot, subscribers: from(u in User, select: u.id))

    for user_id <- bot_with_sub_ids.subscribers do
      Location.refresh_bot_subscriptions(user_id)
    end
  end

  def handle_update(_new, _old), do: :ok
end
