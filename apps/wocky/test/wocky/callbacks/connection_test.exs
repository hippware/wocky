defmodule Wocky.Callbacks.ConnectionTest do
  use Wocky.WatcherCase, async: false

  import Eventually

  alias Faker.Code
  alias Wocky.Callbacks.Connection, as: Callback
  alias Wocky.Factory, as: LocationFactory
  alias Wocky.Location
  alias Wocky.Location.Handler
  alias Wocky.Location.UserLocation.Current
  alias Wocky.Notifier.Push
  alias Wocky.Notifier.Push.Backend.Sandbox
  alias Wocky.Presence.Manager
  alias Wocky.Repo.Factory
  alias Wocky.Repo.Timestamp
  alias Wocky.Roster
  alias Wocky.Test.FakeSocket

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

    on_exit(fn ->
      [sharer, shared_with]
      |> Enum.each(fn u ->
        Handler.stop(u)
        Manager.stop(u)
      end)
    end)

    {:ok, sharer: sharer, shared_with: shared_with}
  end

  test "should increment watcher count when a user connects", ctx do
    FakeSocket.open(ctx.shared_with)

    assert_eventually(get_watcher_count(ctx.sharer) == 1)
  end

  test "should decrement watcher count when a user disconnects", ctx do
    socket = FakeSocket.open(ctx.shared_with)

    assert_eventually(get_watcher_count(ctx.sharer) == 1)

    FakeSocket.close(socket)

    assert_eventually(get_watcher_count(ctx.sharer) == 0)
  end

  describe "push notification" do
    setup %{sharer: sharer} do
      pid = Process.whereis(Dawdle.Client)
      Sandbox.clear_notifications(pid: pid)

      Push.enable(sharer, "testing", Code.isbn13())

      old_value = Sandbox.get_config(:reflect)
      Sandbox.put_config(:reflect, false)

      on_exit(fn -> Sandbox.put_config(:reflect, old_value) end)

      {:ok, pid: pid}
    end

    test "should notify on first connection with no current location", ctx do
      FakeSocket.open(ctx.shared_with)

      assert [n] = Sandbox.wait_notifications(count: 1, pid: ctx.pid)
      assert Map.get(n.payload, "location-request") == 1
    end

    test "should not notify on subsequent connections", ctx do
      FakeSocket.open(ctx.shared_with)

      refute Sandbox.wait_notifications(count: 1, pid: ctx.pid) == []

      Sandbox.clear_notifications(pid: ctx.pid)
      FakeSocket.open(ctx.shared_with)

      assert Sandbox.list_notifications(pid: ctx.pid) == []
    end

    test "should not notify on disconnection", ctx do
      socket = FakeSocket.open(ctx.shared_with)

      refute Sandbox.wait_notifications(count: 1, pid: ctx.pid) == []

      Sandbox.clear_notifications(pid: ctx.pid)
      FakeSocket.close(socket)

      assert Sandbox.list_notifications(pid: ctx.pid) == []
    end

    test "should notify with a stale current location", ctx do
      loc =
        LocationFactory.build(:user_location,
          created_at: Timestamp.shift(seconds: -30)
        )

      Current.set(ctx.sharer, loc)

      FakeSocket.open(ctx.shared_with)

      assert [n] = Sandbox.wait_notifications(count: 1, pid: ctx.pid)
      assert Map.get(n.payload, "location-request") == 1
    end

    test "should not notify with a fresh current location", ctx do
      loc =
        LocationFactory.build(:user_location,
          created_at: Timestamp.shift(seconds: -5)
        )

      Current.set(ctx.sharer, loc)

      FakeSocket.open(ctx.shared_with)

      assert Sandbox.list_notifications(pid: ctx.pid) == []
    end
  end

  defp get_watcher_count(user) do
    %{watchers: count} = Location.get_watched_status(user)
    count
  end
end
