defmodule Wocky.Callbacks.BotItem do
  @moduledoc """
  Callbacks for DB bot item changes
  """

  use DawdleDB.Handler, type: Wocky.Bot.Item

  alias Wocky.Events.BotItem
  alias Wocky.Notifier
  alias Wocky.Repo

  def handle_insert(item) do
    item = Repo.preload(item, [:user, :bot])

    if item.bot && item.user do
      item.bot
      |> recipients(item.user)
      |> Enum.each(&do_notify(&1, item))
    end
  end

  defp recipients(bot, sender) do
    bot = Repo.preload(bot, [:subscribers])

    bot.subscribers
    |> Enum.uniq_by(& &1.id)
    |> Enum.filter(&(&1.id != sender.id))
  end

  defp do_notify(user, item) do
    %BotItem{
      to: user,
      from: item.user,
      item: item
    }
    |> Notifier.notify()
  end
end
