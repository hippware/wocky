defmodule WockyXMPP.BotShareCallbacks do
  @moduledoc """
  Callbacks for DB bot share changes
  """

  use Wocky.Watcher, type: Wocky.Bot.Share, events: [:insert]

  alias Wocky.Repo

  def handle_insert(%Event{action: :insert, new: new}) do
    share = Repo.preload(new, [:user, :sharer, :bot])

    if share.user != nil and share.sharer != nil and share.bot != nil do
      send_notification(share)
    end
  end

  defp send_notification(%{geofence: false} = share) do
    :wocky_bot_users.send_share_notification(
      share.sharer,
      share.user,
      share.bot
    )
  end

  defp send_notification(%{geofence: true} = share) do
    :wocky_bot_users.send_geofence_share_notification(
      share.sharer,
      share.user,
      share.bot
    )
  end
end
