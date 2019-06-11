defmodule Wocky.Callbacks.BotTest do
  use Wocky.WatcherCase

  alias Wocky.Bot
  alias Wocky.Callbacks.Bot, as: Callback
  alias Wocky.GeoUtils
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

    Bot.subscribe(bot, user)

    Bot.update(bot, %{location: GeoUtils.point(1.0, 2.0)})

    assert_eventually(
      (
        %{subscriptions: [bot]} = :sys.get_state(pid)
        Bot.lat(bot) == 1.0 and Bot.lon(bot) == 2.0
      )
    )
  end
end
