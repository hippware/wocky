defmodule Wocky.Callbacks.BotSubscriptionTest do
  use Wocky.WatcherCase

  alias Wocky.Bots
  alias Wocky.Bots.Subscription
  alias Wocky.Callbacks.BotSubscription, as: Callback
  alias Wocky.Location.Handler
  alias Wocky.Repo.Factory
  alias Wocky.Roster

  setup_all do
    Callback.register()
  end

  test "should update the bot subscription cache" do
    owner = Factory.insert(:user)
    user = Factory.insert(:user)

    bot = Factory.insert(:bot, user: owner)

    Roster.befriend(owner, user)

    # Make sure the handler is instantiated
    pid = Handler.get_handler(user)

    Bots.subscribe(bot, user)
    Bots.delete(bot)

    refute_eventually(Subscription.get(user, bot))

    assert_eventually([] == :sys.get_state(pid).subscriptions)
  end
end
