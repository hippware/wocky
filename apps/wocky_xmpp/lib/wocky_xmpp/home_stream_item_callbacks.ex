defmodule WockyXMPP.HomeStreamItemCallbacks do
  @moduledoc """
  Callbacks for HomeStreamItem callbacks
  """

  alias Wocky.HomeStream.Item, as: HomeStreamItem
  alias Wocky.Repo
  alias Wocky.User
  alias Wocky.Watcher.Client
  alias WockyDBWatcher.Event

  def register do
    Client.subscribe(HomeStreamItem, :insert, &handle_change/1)
    Client.subscribe(HomeStreamItem, :update, &handle_change/1)
  end

  def handle_change(%Event{new: new}) do
    item = Repo.preload(new, [:user, :reference_bot])

    if item.user != nil &&
      (item.reference_bot_id == nil || item.reference_bot != nil) do
      :mod_wocky_home_stream.send_notifications(User.to_jid(item.user), item)
    end
  end
end
