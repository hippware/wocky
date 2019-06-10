defmodule Wocky.Callbacks.BotTest do
  use Wocky.WatcherCase

  alias Wocky.Bots
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

    Bots.subscribe(bot, user)

    Bots.update(bot, %{location: GeoUtils.point(1.0, 2.0)})

    assert_eventually(
      (
        %{subscriptions: [bot]} = :sys.get_state(pid)
        Bots.lat(bot) == 1.0 and Bots.lon(bot) == 2.0
      )
    )
  end
end
