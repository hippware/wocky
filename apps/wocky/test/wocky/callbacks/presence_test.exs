defmodule Wocky.Callbacks.PresenceTest do
  use Wocky.WatcherCase

  import Eventually

  alias Wocky.Callbacks.Presence, as: Callback
  alias Wocky.Location
  alias Wocky.Presence
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

  test "should increment watcher count when a user comes online", ctx do
    Presence.publish(ctx.shared_with.id, ctx.shared_with, :online)

    assert_eventually(Location.get_watcher_count(ctx.sharer) == 1)
  end

  test "should decrement watcher count when a user goes offline", ctx do
    Location.inc_watcher_count(ctx.sharer)
    Presence.publish(ctx.shared_with.id, ctx.shared_with, :offline)

    assert_eventually(Location.get_watcher_count(ctx.sharer) == 0)
  end
end
