defmodule Wocky.Repo.HydratorTest do
  use Wocky.DataCase

  alias Wocky.Bot.Subscription
  alias Wocky.Repo.Factory
  alias Wocky.Repo.Hydrator

  setup do
    user = Factory.insert(:user)
    bot = Factory.insert(:bot)
    sub = Factory.insert(:subscription, user: user, bot: bot)
    {:ok, user: user, bot: bot, sub: sub}
  end

  describe "with_assocs/3" do
    test "runs function when struct can be hydrated", ctx do
      sub = Repo.get_by(Subscription, user_id: ctx.user.id)

      assert Hydrator.with_assocs(sub, [:user, :bot], fn rec ->
               rec.user != nil && rec.bot != nil
             end)
    end

    test "fails when record cannot be hyrdated", ctx do
      sub = Repo.get_by(Subscription, user_id: ctx.user.id)

      Repo.delete(ctx.bot)

      assert Hydrator.with_assocs(sub, [:user, :bot], fn rec ->
               rec.user != nil && rec.bot != nil
             end) == :hydration_failed
    end
  end
end
