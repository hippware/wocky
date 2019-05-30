defmodule Wocky.User.LocationShare.CacheTest do
  use Wocky.WatcherCase

  alias Wocky.Repo.{Factory, Timestamp}
  alias Wocky.Roster
  alias Wocky.User.LocationShare
  alias Wocky.User.LocationShare.Cache

  setup do
    [u1, u2, u3] = Factory.insert_list(3, :user)
    Roster.befriend(u1, u2)
    Roster.befriend(u1, u3)
    {:ok, u1: u1, u2: u2, u3: u3}
  end

  describe "add/remove users" do
    test "adding users", ctx do
      assert {:ok, _} =
               LocationShare.start_sharing_location(
                 ctx.u1,
                 ctx.u2,
                 Timestamp.shift(days: 1)
               )

      assert_eventually([ctx.u2.id] == Cache.get(ctx.u1.id))

      assert {:ok, _} =
               LocationShare.start_sharing_location(
                 ctx.u1,
                 ctx.u3,
                 Timestamp.shift(days: 1)
               )

      assert_eventually(
        [ctx.u2.id, ctx.u3.id] |> Enum.sort() ==
          ctx.u1.id |> Cache.get() |> Enum.sort()
      )
    end

    test "removing users", ctx do
      assert {:ok, _} =
               LocationShare.start_sharing_location(
                 ctx.u1,
                 ctx.u2,
                 Timestamp.shift(days: 1)
               )

      assert_eventually([ctx.u2.id] == Cache.get(ctx.u1.id))

      LocationShare.stop_sharing_location(ctx.u1)
      assert_eventually([] == Cache.get(ctx.u1.id))
    end
  end

  describe "expiry" do
    test "simple expiry", ctx do
      assert {:ok, _} =
               LocationShare.start_sharing_location(
                 ctx.u1,
                 ctx.u2,
                 Timestamp.shift(seconds: 2)
               )

      assert_eventually([ctx.u2.id] == Cache.get(ctx.u1.id))
      Process.sleep(2000)

      assert [] = Cache.get(ctx.u1.id)
    end

    test "updated expiry", ctx do
      assert {:ok, _} =
               LocationShare.start_sharing_location(
                 ctx.u1,
                 ctx.u2,
                 Timestamp.shift(seconds: 2)
               )

      assert_eventually([ctx.u2.id] == Cache.get(ctx.u1.id))

      assert {:ok, _} =
               LocationShare.start_sharing_location(
                 ctx.u1,
                 ctx.u2,
                 Timestamp.shift(seconds: 3)
               )

      Process.sleep(2000)

      assert [ctx.u2.id] == Cache.get(ctx.u1.id)
      Process.sleep(1000)

      assert [] = Cache.get(ctx.u1.id)
    end
  end

  describe "refresh" do
    test "cache reload on get if not primed", ctx do
      assert {:ok, _} =
               LocationShare.start_sharing_location(
                 ctx.u1,
                 ctx.u2,
                 Timestamp.shift(days: 1)
               )

      assert_eventually([ctx.u2.id] == Cache.get(ctx.u1.id))

      {:ok, _} = Redix.command(Redix, ["DEL", Cache.key(ctx.u1.id)])

      # Calling 'get' should force the cache to refresh
      assert [ctx.u2.id] == Cache.get(ctx.u1.id)
    end
  end
end
