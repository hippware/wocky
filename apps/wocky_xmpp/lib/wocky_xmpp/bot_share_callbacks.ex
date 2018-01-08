defmodule WockyXMPP.BotShareCallbacks do
  @moduledoc """
  Callback handler for DB bot share changes
  """

  use GenStage

  alias Ecto.Adapters.SQL.Sandbox
  alias Wocky.Bot.Share
  alias Wocky.Repo
  alias WockyDBWatcher.Watcher

  def start_link, do: GenStage.start_link(__MODULE__, nil, name: __MODULE__)

  def init(_) do
    {:ok, watcher} = WockyDBWatcher.start_watcher(
                      Share, :insert, "bot_share_callbacks")
    {:consumer, nil, subscribe_to: [watcher]}
  end

  def handle_events([], _from, state), do: {:noreply, [], state}
  def handle_events(
    [%Watcher{action: :insert, new: new} | tail], from, state) do
      share = maybe_unbox(fn() -> Repo.preload(new, [:user, :sharer, :bot]) end)

      :wocky_bot_users.send_share_notification(
                      share.sharer, share.user, share.bot)
      handle_events(tail, from, state)
  end

  # This is an icky hack because for some reason that I don't understand, this
  # process can't access the sandbox connections in testing. There has to be a
  # better way, but I couldn't find it. If/when we have more callback handlers
  # also doing DB access, we'll probably want to move this somewhere more
  # general.
  defp maybe_unbox(fun) do
    case Keyword.get(Repo.config(), :pool) do
      Sandbox -> Sandbox.unboxed_run(Repo, fun)
      _ -> fun.()
    end
  end
end
