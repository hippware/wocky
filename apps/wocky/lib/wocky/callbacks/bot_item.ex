defmodule Wocky.Callbacks.BotItem do
  @moduledoc """
  Callbacks for DB bot item changes
  """

  use DawdleDB.Handler, type: Wocky.Bot.Item

  alias Wocky.Events.BotItem
  alias Wocky.Notifier
  alias Wocky.Repo
  alias Wocky.Repo.Hydrator

  def handle_insert(new) do
    Hydrator.with_assocs(new, [:user, :bot], fn rec ->
      rec.bot
      |> recipients(rec.user)
      |> Enum.each(&do_notify(&1, rec))
    end)
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
