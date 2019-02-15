defmodule Wocky.Callbacks.LocationShare do
  @moduledoc "DB callback handler for location shares"

  use Wocky.Watcher, type: Wocky.User.LocationShare, events: [:insert, :delete]

  alias Wocky.Repo
  alias Wocky.User.Notification.{LocationShare, LocationShareEnd}

  def handle_insert(%Event{new: new}) do
    new = Repo.preload(new, [:user, :shared_with])

    if new.user != nil && new.shared_with != nil do
      LocationShare.notify(new)
    end
  end

  def handle_delete(%Event{old: old}) do
    old = Repo.preload(old, [:user, :shared_with])

    if old.user != nil && old.shared_with != nil do
      LocationShareEnd.notify(old)
    end
  end
end
