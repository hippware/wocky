defmodule Wocky.Callbacks.BotTest do
  use Wocky.WatcherCase

  alias Wocky.Callbacks.Bot, as: BotCallback
  alias Wocky.Callbacks.BotSubscription, as: SubCallback
  alias Wocky.Contacts
  alias Wocky.GeoUtils
  alias Wocky.Location.Handler
  alias Wocky.POI
  alias Wocky.Relation

  setup_all do
    BotCallback.register()
    SubCallback.register()
  end

  test "should update the bot subscription cache" do
    owner = Factory.insert(:user)
    user = Factory.insert(:user)

    bot = Factory.insert(:bot, user: owner)

    Contacts.befriend(owner, user)

    # Make sure the handler is instantiated
    pid = Handler.get_handler(user)

    Relation.subscribe(user, bot)

    POI.update(bot, %{location: GeoUtils.point(1.0, 2.0)})

    assert_eventually(
      (
        subs = :sys.get_state(pid).bot_subscriptions

        length(subs) == 1 and
          POI.lat(hd(subs)) == 1.0 and
          POI.lon(hd(subs)) == 2.0
      )
    )
  end
end
