defmodule Wocky.Callbacks.RosterItem do
  @moduledoc """
  Callbacks for roster item changes
  """

  use Wocky.Watcher, type: Wocky.Roster.Item, events: [:insert, :update]

  alias Wocky.Push
  alias Wocky.Push.Events.NewFollowerEvent
  alias Wocky.Repo
  alias Wocky.Roster
  alias Wocky.User.Notification.UserFollow

  def handle_insert(%Event{new: new}) do
    if Roster.follower?(new) do
      notify_of_new_follower(new)
    end
  end

  def handle_update(%Event{old: old, new: new}) do
    if not Roster.follower?(old) and Roster.follower?(new) do
      notify_of_new_follower(new)
    end
  end

  defp notify_of_new_follower(item) do
    item = Repo.preload(item, [:user, :contact])

    if not (is_nil(item.user) or is_nil(item.contact)) do
      UserFollow.notify(item.contact, item.user)

      event = NewFollowerEvent.new(user: item.user, follower: item.contact)
      Push.notify_all(item.user, event)
    end
  end
end
