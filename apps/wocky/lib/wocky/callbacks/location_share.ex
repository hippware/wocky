defmodule Wocky.Callbacks.LocationShare do
  @moduledoc "DB callback handler for location shares"

  use Wocky.Watcher, type: Wocky.User.LocationShare, events: [:insert]

  alias Wocky.Repo
  alias Wocky.User.Notification.LocationShare

  def handle_insert(%Event{new: new}) do
    new = Repo.preload(new, [:user, :shared_with])

    if new.user != nil && new.shared_with != nil do
      LocationShare.notify(new)
    end
  end
end
