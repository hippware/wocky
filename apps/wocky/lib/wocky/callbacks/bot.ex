defmodule Wocky.Callbacks.Bot do
  @moduledoc """
  Callbacks for DB bot changes
  """

  use Wocky.Watcher, type: Wocky.Bot, events: [:insert, :update]

  alias Wocky.Bot
  alias Wocky.Repo
  alias Wocky.Waiter

  def handle_insert(%Event{action: :insert, new: new}) do
    update_owner_subscription(new)
  end

  def handle_update(%Event{action: :update, old: old, new: new}) do
    Bot.maybe_update_hs_items(old, new)
  end

  defp update_owner_subscription(bot) do
    %{user: user} = Repo.preload(bot, [:user])

    if user != nil do
      Bot.subscribe(bot, user)

      bot
      |> Bot.sub_setup_event()
      |> Waiter.notify()
    end
  end
end
