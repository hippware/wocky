defmodule Wocky.Location.Share.CacheTest do
  use Wocky.DataCase, async: true

  alias Wocky.Location.Share.Cache
  alias Wocky.Repo.Factory
  alias Wocky.Repo.ID

  describe "refresh/1" do
    setup do
      share1 = Factory.insert(:roster_item)
      share2 = Factory.insert(:roster_item, user: share1.user)

      {:ok,
       user: share1.user,
       shared_with1: share1.contact,
       shared_with2: share2.contact}
    end

    test "should return the found user ids", ctx do
      assert [ctx.shared_with1.id, ctx.shared_with2.id] |> Enum.sort() ==
               ctx.user.id |> Cache.refresh() |> Enum.sort()
    end

    test "should update the cache", ctx do
      results = Cache.refresh(ctx.user.id)

      assert results == Cache.get(ctx.user.id)
    end

    test "should not load disabled shares", ctx do
      share =
        Factory.insert(:roster_item, share_type: :disabled, user: ctx.user)

      results = Cache.refresh(ctx.user.id)

      refute Enum.member?(results, share.contact.id)
    end

    test "should do nothing for nonexistant user" do
      id = ID.new()

      assert [] == Cache.refresh(id)
      assert [] == Cache.get(id)
    end

    test "handles old cache entries appropriately", ctx do
      id = ID.new()

      Cache.put(ctx.user.id, [{id, DateTime.utc_now()}])

      assert Cache.get(ctx.user.id) == [id]
    end
  end

  describe "get/1" do
    test "should refresh the cache automatically" do
      share = Factory.insert(:roster_item)

      assert [share.contact.id] == Cache.get(share.user.id)
    end
  end
end
