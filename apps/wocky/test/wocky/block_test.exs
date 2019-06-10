defmodule Wocky.BlockTest do
  use Wocky.DataCase, async: true

  alias Wocky.Block
  alias Wocky.Bots
  alias Wocky.Repo.Factory

  setup do
    [u1, u2, u3] = Factory.insert_list(3, :user)
    bot = Factory.insert(:bot, user: u1)

    {:ok, user1: u1, user2: u2, user3: u3, bot: bot}
  end

  describe "basic functions" do
    test "block/2 should be bi-directional", %{user1: u1, user2: u2} do
      Block.block(u1, u2)

      assert Block.blocked?(u1, u2)
      assert Block.blocked?(u2, u1)
    end

    test "unblock/2 should remove a block", %{user1: u1, user2: u2} do
      Block.block(u1, u2)
      Block.unblock(u1, u2)

      refute Block.blocked?(u1, u2)
      refute Block.blocked?(u2, u1)
    end

    test "block should remain if both users block and one unblocks", %{
      user1: u1,
      user2: u2
    } do
      Block.block(u1, u2)
      Block.block(u2, u1)
      Block.unblock(u1, u2)

      assert Block.blocked?(u1, u2)
      assert Block.blocked?(u2, u1)
    end

    test "blocks_query/1", %{user1: u1, user2: u2} do
      Block.block(u1, u2)

      assert [block] = u1.id |> Block.blocks_query() |> Repo.all()
      assert block.blocker_id == u1.id
      assert block.blockee_id == u2.id
    end

    test "object_visible_query/3", ctx do
      Block.block(ctx.user1, ctx.user2)

      query =
        ctx.bot.id
        |> Bots.get_query()
        |> Block.object_visible_query(ctx.user2)

      assert is_nil(Repo.one(query))

      Block.unblock(ctx.user1, ctx.user2)

      refute is_nil(Repo.one(query))
    end
  end
end
