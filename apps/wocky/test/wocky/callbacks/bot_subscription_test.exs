defmodule Wocky.Callbacks.BotSubscriptionTest do
  use Wocky.WatcherCase

  alias Wocky.Callbacks.BotSubscription, as: Callback
  alias Wocky.Contacts
  alias Wocky.Location.Handler
  alias Wocky.POI
  alias Wocky.Relation

  setup_all do
    Callback.register()
  end

  describe "bot subscription hooks" do
    setup do
      owner = Factory.insert(:user)
      user = Factory.insert(:user)

      bot = Factory.insert(:bot, user: owner)

      Contacts.befriend(owner, user)

      pid = Handler.get_handler(user)

      Relation.subscribe(user, bot)

      {:ok, pid: pid, user: user, bot: bot}
    end

    test "should add a new bot subscription", %{bot: bot, pid: pid} do
      assert_eventually(
        (
          subs = :sys.get_state(pid).bot_subscriptions
          Enum.all?(subs, &(&1.id == bot.id)) and length(subs) == 1
        )
      )
    end

    test "should remove a bot subscription on unsubscribe", %{
      user: user,
      bot: bot,
      pid: pid
    } do
      Relation.unsubscribe(user, bot)

      assert_eventually(%{bot_subscriptions: []} = :sys.get_state(pid))
    end

    test "should remove a bot subscription on bot delete", %{
      user: user,
      bot: bot,
      pid: pid
    } do
      POI.delete(bot)

      refute_eventually(Relation.subscribed?(user, bot))

      assert_eventually(%{bot_subscriptions: []} = :sys.get_state(pid))
    end
  end
end
