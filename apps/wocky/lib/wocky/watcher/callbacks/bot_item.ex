defmodule Wocky.Watcher.Callbacks.BotItem do
  @moduledoc """
  Callbacks for DB bot item changes
  """

  alias Wocky.Bot.Item
  alias Wocky.HomeStreamItem
  alias Wocky.Repo
  alias Wocky.Watcher.Client
  alias WockyDBWatcher.Event

  def register do
    Client.subscribe(Item, :insert, &handle_insert/1)
    Client.subscribe(Item, :delete, &handle_delete/1)
  end

  def handle_insert(%Event{new: new}) do
    maybe_update_hs(new)
  end

  def handle_delete(%Event{old: old}) do
    maybe_update_hs(old)
  end

  defp maybe_update_hs(item) do
    %{bot: bot} = Repo.preload(item, [:bot])

    if bot != nil do
      HomeStreamItem.update_ref_bot(bot)
    end
  end
end
