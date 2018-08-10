defmodule Wocky.Callbacks.BotItem do
  @moduledoc """
  Callbacks for DB bot item changes
  """

  use Wocky.Watcher, type: Wocky.Bot.Item, events: [:insert, :delete]

  alias Wocky.HomeStream
  alias Wocky.Repo

  def handle_insert(%Event{new: new}) do
    maybe_update_hs(new)
  end

  def handle_delete(%Event{old: old}) do
    maybe_update_hs(old)
  end

  defp maybe_update_hs(item) do
    %{bot: bot} = Repo.preload(item, [:bot])

    if bot != nil do
      HomeStream.update_ref_bot(bot)
    end
  end
end
