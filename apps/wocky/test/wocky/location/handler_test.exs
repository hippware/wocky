defmodule Wocky.Location.HandlerTest do
  use Wocky.WatcherCase

  import Eventually

  alias Wocky.Callbacks.BotSubscription
  alias Wocky.Location.Handler
  alias Wocky.Relation
  alias Wocky.Repo.Factory
  alias Wocky.Roster

  setup_all do
    BotSubscription.register()
  end

  describe "get_handler/1" do
    test "both versions should get the same handler" do
      user = Factory.insert(:user)

      assert Handler.get_handler(user.id) == Handler.get_handler(user)
    end
  end

  describe "bot subscription hooks" do
    setup do
      owner = Factory.insert(:user)
      user = Factory.insert(:user)

      bot = Factory.insert(:bot, user: owner)

      Roster.befriend(owner, user)

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

    test "should remove a bot subscription", %{user: user, bot: bot, pid: pid} do
      Relation.unsubscribe(user, bot)

      assert_eventually(%{bot_subscriptions: []} = :sys.get_state(pid))
    end
  end
end
