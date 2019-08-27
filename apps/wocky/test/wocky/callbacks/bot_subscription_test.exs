defmodule Wocky.Callbacks.BotSubscriptionTest do
  use Wocky.WatcherCase

  alias Wocky.Callbacks.BotSubscription, as: Callback
  alias Wocky.Location.Handler
  alias Wocky.POI
  alias Wocky.Relation
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

    Relation.subscribe(user, bot)
    POI.delete(bot)

    refute_eventually(Relation.subscribed?(user, bot))

    assert_eventually([] == :sys.get_state(pid).bot_subscriptions)
  end
end
