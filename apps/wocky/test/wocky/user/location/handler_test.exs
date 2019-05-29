defmodule Wocky.User.Location.HandlerTest do
  use Wocky.WatcherCase

  alias Wocky.Bot
  alias Wocky.Bot.Subscription
  alias Wocky.GeoUtils
  alias Wocky.Repo.Factory
  alias Wocky.Roster
  alias Wocky.User.Location.Handler

  setup do
    owner = Factory.insert(:user)
    user = Factory.insert(:user)

    bot = Factory.insert(:bot, user: owner)

    Roster.befriend(owner, user)

    pid = Handler.get_handler(user.id)

    Bot.subscribe(bot, user)

    {:ok, pid: pid, user: user, bot: bot}
  end

  describe "bot subscription hooks" do
    test "should add a new bot subscription", %{bot: bot, pid: pid} do
      assert %{subscriptions: [^bot]} = :sys.get_state(pid)
    end

    test "should remove a bot subscription", %{user: user, bot: bot, pid: pid} do
      Bot.unsubscribe(bot, user)

      assert %{subscriptions: []} = :sys.get_state(pid)
    end
  end

  describe "bot update callback" do
    test "should update the bot subscription", %{bot: bot, pid: pid} do
      Bot.update(bot, %{location: GeoUtils.point(1.0, 2.0)})

      assert_eventually(
        (
          %{subscriptions: [bot]} = :sys.get_state(pid)
          Bot.lat(bot) == 1.0 and Bot.lon(bot) == 2.0
        )
      )
    end
  end

  describe "subscription delete callback" do
    test "should remove the bot subscription", %{user: user, bot: bot, pid: pid} do
      Bot.delete(bot)

      refute_eventually(Subscription.get(user, bot))

      assert_eventually([] == :sys.get_state(pid).subscriptions)
    end
  end
end
