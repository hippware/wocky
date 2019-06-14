defmodule Wocky.Relations.RelationsTest do
  use Wocky.DataCase, async: true

  alias Wocky.Bots
  alias Wocky.Bots.Invitation
  alias Wocky.Relations
  # alias Wocky.Repo
  alias Wocky.Repo.Factory
  alias Wocky.Roster

  setup do
    user = Factory.insert(:user, device: "testing")

    {:ok,
     user: user,
     id: user.id,
     external_id: user.external_id,
     phone_number: user.phone_number}
  end


  describe "bot relationships" do
    setup ctx do
      other_user = Factory.insert(:user)
      Roster.befriend(ctx.user, other_user)

      owned_bot = Factory.insert(:bot, user: ctx.user)
      pending_bot = Factory.insert(:bot, user: ctx.user, pending: true)
      invited_bot = Factory.insert(:bot, user: other_user)
      subscribed_bot = Factory.insert(:bot, user: other_user)
      unaffiliated_bot = Factory.insert(:bot, user: other_user)

      Invitation.put(ctx.user, invited_bot, other_user)
      Bots.subscribe(subscribed_bot, ctx.user)

      {:ok,
       other_user: other_user,
       owned_bot: owned_bot,
       pending_bot: pending_bot,
       invited_bot: invited_bot,
       subscribed_bot: subscribed_bot,
       unaffiliated_bot: unaffiliated_bot}
    end

    test "can_access?/2", ctx do
      assert Relations.can_access?(ctx.user, ctx.owned_bot)
      assert Relations.can_access?(ctx.user, ctx.invited_bot)
      refute Relations.can_access?(ctx.user, ctx.unaffiliated_bot)
    end

    test "get_subscriptions/1", ctx do
      subscriptions = Relations.get_subscriptions(ctx.user)

      assert length(subscriptions) == 1
      assert Enum.any?(subscriptions, &same_bot(&1, ctx.subscribed_bot))
      refute Enum.any?(subscriptions, &same_bot(&1, ctx.owned_bot))
      refute Enum.any?(subscriptions, &same_bot(&1, ctx.pending_bot))
    end

    test "get_owned_bots/1", ctx do
      bots = Relations.get_owned_bots(ctx.user)

      assert length(bots) == 1
      assert Enum.any?(bots, &same_bot(&1, ctx.owned_bot))
      refute Enum.any?(bots, &same_bot(&1, ctx.pending_bot))
    end
  end

  defp same_bot(bot1, bot2), do: bot1.id == bot2.id
end
