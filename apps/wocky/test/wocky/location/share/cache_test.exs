defmodule Wocky.Location.Share.CacheTest do
  use Wocky.DataCase, async: true

  import Eventually

  alias Wocky.Location.Share.Cache
  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID
  alias Wocky.Repo.Timestamp

  describe "refresh/1" do
    setup do
      share1 = Factory.insert(:user_location_share)
      share2 = Factory.insert(:user_location_share, user: share1.user)

      {:ok,
       user: share1.user,
       shared_with1: share1.shared_with,
       shared_with2: share2.shared_with}
    end

    test "should return the found user ids", ctx do
      assert [ctx.shared_with1.id, ctx.shared_with2.id] |> Enum.sort() ==
               ctx.user.id |> Cache.refresh() |> Enum.sort()
    end

    test "should update the cache", ctx do
      results = Cache.refresh(ctx.user.id)

      assert results == Cache.get(ctx.user.id)
    end

    test "should not load expired shares", ctx do
      share =
        Factory.insert(:user_location_share,
          expires_at: Timestamp.shift(seconds: -1),
          user: ctx.user
        )

      results = Cache.refresh(ctx.user.id)

      refute Enum.member?(results, share.shared_with.id)
    end

    test "should do nothing for nonexistant user" do
      id = ID.new()

      assert [] == Cache.refresh(id)
      assert [] == Cache.get(id)
    end
  end

  describe "get/1" do
    test "should refresh the cache automatically" do
      share = Factory.insert(:user_location_share)

      assert [share.shared_with.id] == Cache.get(share.user.id)
    end

    test "should not return expired shares" do
      share =
        Factory.insert(:user_location_share,
          expires_at: Timestamp.shift(seconds: 2)
        )

      assert [share.shared_with.id] == Cache.refresh(share.user.id)

      assert_eventually([] == Cache.get(share.user.id))
    end
  end
end
