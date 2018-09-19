defmodule Wocky.Callbacks.BotItem do
  @moduledoc """
  Callbacks for DB bot item changes
  """

  use Wocky.Watcher, type: Wocky.Bot.Item, events: [:insert, :delete]

  alias Wocky.HomeStream
  alias Wocky.Repo
  alias Wocky.User.Notification.BotItem

  def handle_insert(%Event{new: new}) do
    item = maybe_notify(new)

    if item.bot != nil do
      BotItem.notify(item)
    end
  end

  def handle_delete(%Event{old: old}) do
    maybe_notify(old)
  end

  defp maybe_notify(item) do
    %{bot: bot} = item = Repo.preload(item, [:bot])

    if bot != nil do
      HomeStream.update_ref_bot(bot)
    end

    item
  end
end
