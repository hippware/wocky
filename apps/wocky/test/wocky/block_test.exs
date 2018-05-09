defmodule Wocky.BlockTest do
  use Wocky.DataCase

  alias Wocky.Block
  alias Wocky.Bot
  alias Wocky.Collections
  alias Wocky.Repo.Factory

  setup do
    [u1, u2, u3] = Factory.insert_list(3, :user)
    bot = Factory.insert(:bot, user: u1)

    {:ok, user1: u1, user2: u2, user3: u3, bot: bot}
  end

  test "block/2 should be bi-directional", %{user1: u1, user2: u2} do
    Block.block(u1, u2)

    assert Block.blocked?(u1.id, u2.id)
    assert Block.blocked?(u2.id, u1.id)
  end

  test "unblock/2 should remove a block", %{user1: u1, user2: u2} do
    Block.block(u1, u2)
    Block.unblock(u1, u2)

    refute Block.blocked?(u1.id, u2.id)
    refute Block.blocked?(u2.id, u1.id)
  end

  test "block should remain if both users block and one unblocks", %{
    user1: u1,
    user2: u2
  } do
    Block.block(u1, u2)
    Block.block(u2, u1)
    Block.unblock(u1, u2)

    assert Block.blocked?(u1.id, u2.id)
    assert Block.blocked?(u2.id, u1.id)
  end

  test "object_visible_query/3", shared do
    Block.block(shared.user1, shared.user2)

    query =
      shared.bot.id
      |> Bot.get_query()
      |> Block.object_visible_query(shared.user2.id)

    assert is_nil(Repo.one(query))

    Block.unblock(shared.user1, shared.user2)

    refute is_nil(Repo.one(query))
  end

  test "assoc_object_visible_query/3", shared do
    collection = Factory.insert(:collection, user: shared.user3)
    Collections.subscribe(collection.id, shared.user1)

    Block.block(shared.user1, shared.user2)

    query =
      collection
      |> Ecto.assoc(:subscribers)
      |> Block.assoc_object_visible_query(shared.user2.id, :id)

    assert Repo.all(query) == []

    Block.unblock(shared.user1, shared.user2)

    assert length(Repo.all(query)) == 1
  end
end
