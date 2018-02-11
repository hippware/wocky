defmodule WockyXMPP.BotShareCallbacks do
  @moduledoc """
  Callbacks for DB bot share changes
  """

  alias Wocky.Bot.Share
  alias Wocky.Repo
  alias Wocky.Watcher.Client
  alias WockyDBWatcher.Event

  def register do
    Client.subscribe(Share, :insert, &handle_insert/1)
  end

  def handle_insert(%Event{action: :insert, new: new}) do
    share = Repo.preload(new, [:user, :sharer, :bot])

    if share.user != nil and share.sharer != nil and share.bot != nil do
      :wocky_bot_users.send_share_notification(
        share.sharer,
        share.user,
        share.bot
      )
    end
  end
end
