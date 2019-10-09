defmodule Wocky.Callbacks.ConnectionTest do
  use Wocky.WatcherCase, async: false

  import Eventually
  import Wocky.Presence.TestHelper

  alias Wocky.Callbacks.Connection, as: Callback
  alias Wocky.Location
  alias Wocky.Repo.Factory
  alias Wocky.Repo.Timestamp
  alias Wocky.Roster

  setup_all do
    Callback.register()
  end

  setup do
    sharer = Factory.insert(:user)
    shared_with = Factory.insert(:user)

    Roster.befriend(sharer, shared_with)

    expiry =
      Timestamp.shift(days: 5)
      |> DateTime.truncate(:second)

    Location.start_sharing_location(sharer, shared_with, expiry)

    {:ok, sharer: sharer, shared_with: shared_with}
  end

  test "should increment watcher count when a user connects", ctx do
    {_, _} = connect(ctx.shared_with)

    assert_eventually(get_watcher_count(ctx.sharer) == 1)
  end

  test "should decrement watcher count when a user disconnects", ctx do
    {pid, _} = connect(ctx.shared_with)

    assert_eventually(get_watcher_count(ctx.sharer) == 1)

    disconnect(pid)

    assert_eventually(get_watcher_count(ctx.sharer) == 0)
  end

  defp get_watcher_count(user) do
    %{watchers: count} = Location.get_watched_status(user)
    count
  end
end
