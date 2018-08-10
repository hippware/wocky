defmodule WockyXMPP.HomeStreamItemCallbacks do
  @moduledoc """
  Callbacks for HomeStreamItem callbacks
  """

  use Wocky.Watcher, type: Wocky.HomeStream.Item, events: [:insert, :update]

  alias Wocky.Repo
  alias Wocky.User

  def handle_insert(event), do: handle_change(event)

  def handle_update(event), do: handle_change(event)

  defp handle_change(%Event{new: new}) do
    item = Repo.preload(new, [:user, :reference_bot])

    if item.user != nil &&
         (item.reference_bot_id == nil || item.reference_bot != nil) do
      :mod_wocky_home_stream.send_notifications(User.to_jid(item.user), item)
    end
  end
end
