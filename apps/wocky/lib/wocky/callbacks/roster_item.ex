defmodule Wocky.Callbacks.RosterItem do
  @moduledoc """
  Callbacks for roster item changes
  """

  use Wocky.Watcher, type: Wocky.Roster.Item, events: [:delete]

  alias Wocky.Repo
  alias Wocky.User

  def handle_delete(%Event{old: old}) do
    item = Repo.preload(old, [:user, :contact])

    # Cancel location sharing
    if item.user != nil and item.contact != nil do
      User.stop_sharing_location(item.user, item.contact)
      User.stop_sharing_location(item.contact, item.user)
    end

    :ok
  end
end
