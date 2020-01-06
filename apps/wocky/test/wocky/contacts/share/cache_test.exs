defmodule Wocky.Contacts.Share.CacheTest do
  use Wocky.DataCase, async: true

  alias Wocky.Contacts.Share.Cache
  alias Wocky.Contacts.Share.CachedRelationship
  alias Wocky.Repo.ID

  describe "refresh/1" do
    setup do
      share1 = Factory.insert(:friend)
      share2 = Factory.insert(:friend, user: share1.user)

      {:ok,
       user: share1.user,
       shared_with1: CachedRelationship.new(share1),
       shared_with2: CachedRelationship.new(share2)}
    end

    test "should return the found user ids", ctx do
      assert [ctx.shared_with1, ctx.shared_with2] |> Enum.sort() ==
               ctx.user.id |> Cache.refresh() |> Enum.sort()
    end

    test "should update the cache", ctx do
      results = Cache.refresh(ctx.user.id)

      assert results == Cache.get(ctx.user.id)
    end

    test "should not load disabled shares", ctx do
      share = Factory.insert(:friend, share_type: :disabled, user: ctx.user)

      results = Cache.refresh(ctx.user.id)

      refute Enum.member?(results, share.contact)
    end

    test "should do nothing for nonexistant user" do
      id = ID.new()

      assert [] == Cache.refresh(id)
      assert [] == Cache.get(id)
    end

    test "handles old cache entries with invalid ID appropriately", ctx do
      id = ID.new()

      Cache.put(ctx.user.id, [id])

      assert ctx.user.id |> Cache.get() |> Enum.sort() ==
               [ctx.shared_with1, ctx.shared_with2] |> Enum.sort()
    end

    test "handles older cache entries with invalid ID appropriately", ctx do
      id = ID.new()

      Cache.put(ctx.user.id, [{id, DateTime.utc_now()}])

      assert ctx.user.id |> Cache.get() |> Enum.sort() ==
               [ctx.shared_with1, ctx.shared_with2] |> Enum.sort()
    end
  end

  describe "get/1" do
    test "should refresh the cache automatically" do
      share = Factory.insert(:friend)

      assert [CachedRelationship.new(share)] == Cache.get(share.user.id)
    end
  end
end
