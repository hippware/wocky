defmodule Wocky.Location.LocationTest do
  use Wocky.DataCase, async: false

  alias Wocky.Location
  alias Wocky.Location.BotEvent
  alias Wocky.Location.UserLocation
  alias Wocky.POI
  alias Wocky.Relation
  alias Wocky.Repo.Factory
  alias Wocky.Roster

  setup do
    user = Factory.insert(:user)

    {:ok, user: user, id: user.id}
  end

  describe "set_location/2" do
    setup ctx do
      user2 = Factory.insert(:user)
      bot = Factory.insert(:bot, user: user2)

      Roster.befriend(ctx.user, user2)
      Relation.subscribe(ctx.user, bot)

      {:ok, bot: bot, lat: POI.lat(bot), lon: POI.lon(bot)}
    end

    test "should initiate geofence processing", ctx do
      location =
        UserLocation.new(%{
          lat: ctx.lat,
          lon: ctx.lon,
          accuracy: 10,
          device: "testing"
        })

      assert {:ok, _} = Location.set_user_location(ctx.user, location)
      assert BotEvent.get_last_event_type(ctx.id, ctx.bot.id) == :transition_in
    end
  end

  describe "set_location_for_bot/3" do
    setup ctx do
      user2 = Factory.insert(:user)
      bot = Factory.insert(:bot, user: user2)

      Roster.befriend(ctx.user, user2)
      Relation.subscribe(ctx.user, bot)

      location =
        UserLocation.new(%{
          lat: POI.lat(bot),
          lon: POI.lon(bot),
          accuracy: 10,
          device: "testing"
        })

      {:ok, bot: bot, location: location}
    end

    test "should initiate geofence processing for that bot", ctx do
      assert {:ok, _} =
               Location.set_user_location_for_bot(
                 ctx.user,
                 ctx.location,
                 ctx.bot
               )

      assert Relation.visiting?(ctx.user, ctx.bot)
    end
  end

  describe "get_current_location/1" do
    test "should return the user's current location if known", ctx do
      location = Factory.build(:location)
      {:ok, _} = Location.set_user_location(ctx.user, location)

      loc2 = Location.get_current_user_location(ctx.user)
      assert loc2
      assert loc2.lat == location.lat
      assert loc2.lon == location.lon
      assert loc2.accuracy == location.accuracy
    end

    test "should return nil if the user's location is unknown", ctx do
      refute Location.get_current_user_location(ctx.user)
    end
  end

  describe "watcher count" do
    test "should increment and decrement the watcher count", %{user: user} do
      assert %{watchers: 0} = Location.get_watched_status(user)

      :ok = Location.inc_watcher_count(user)

      assert %{watchers: 1} = Location.get_watched_status(user)

      :ok = Location.inc_watcher_count(user)

      assert %{watchers: 2} = Location.get_watched_status(user)

      :ok = Location.dec_watcher_count(user)

      assert %{watchers: 1} = Location.get_watched_status(user)

      :ok = Location.dec_watcher_count(user)

      assert %{watchers: 0} = Location.get_watched_status(user)
    end

    test "should not go negative", %{user: user} do
      assert %{watchers: 0} = Location.get_watched_status(user)

      :ok = Location.dec_watcher_count(user)

      assert %{watchers: 0} = Location.get_watched_status(user)
    end
  end
end
