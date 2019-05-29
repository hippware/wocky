defmodule Wocky.Callbacks.Bot do
  @moduledoc """
  Callbacks for DB bot changes
  """

  use DawdleDB.Handler, type: Wocky.Bot

  alias Wocky.{Bot, Location, Repo, Waiter}

  def handle_insert(new) do
    %{user: user} = Repo.preload(new, [:user])

    if user != nil do
      :ok = Bot.subscribe(new, user)

      new
      |> Bot.sub_setup_event()
      |> Waiter.notify()
    end
  end

  def handle_update(%Bot{location: new} = bot, %Bot{location: old})
      when new != old do
    bot_with_subs = Repo.preload(bot, [:subscribers])

    for user <- bot_with_subs.subscribers do
      Location.add_subscription(user, bot)
    end
  end

  def handle_update(_new, _old), do: :ok
end
