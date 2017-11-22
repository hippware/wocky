defmodule Wocky.User.LocationTest do
  use Wocky.DataCase
  use Wocky.JID

  alias Faker.Address
  alias Faker.Code
  alias Timex.Duration
  alias Wocky.Bot
  alias Wocky.Bot.Subscription
  alias Wocky.Push
  alias Wocky.Push.Sandbox
  alias Wocky.Repo.Factory
  alias Wocky.User.BotEvent
  alias Wocky.User.Location

  @rsrc "testing"
  @platform "apple"

  setup do
    {:ok, _pid} = Sandbox.start_link

    owner = Factory.insert(:user)
    Push.enable(owner.id, @rsrc, @platform, Code.isbn13)

    user = Factory.insert(:user)
    Push.enable(user.id, @rsrc, @platform, Code.isbn13)

    bot_list = Factory.insert_list(3, :bot, user: owner)
    bot = hd(bot_list)

    :ok = Subscription.put(user, bot)

    {:ok, owner: owner, user: user, bot: bot, bot_list: bot_list}
  end

  describe "changeset/2 validations" do
    setup %{user: user} do
      {:ok, loc: %Location{user: user}}
    end

    test "should pass with valid attributes", %{loc: loc} do
      data = %{resource: "testing", lat: 1.0, lon: 1.0, accuracy: 10}
      changeset = Location.changeset(loc, data)
      assert changeset.valid?
    end

    test "should fail if fields are missing", %{loc: loc} do
      changeset = Location.changeset(loc, %{})
      refute changeset.valid?
      for a <- [:resource, :lat, :lon, :accuracy] do
        assert "can't be blank" in errors_on(changeset)[a]
      end
    end

    test "should fail if the accuracy is negative", %{loc: loc} do
      data = %{resource: "testing", lat: 1.0, lon: 1.0, accuracy: -1}
      changeset = Location.changeset(loc, data)
      assert errors_on(changeset)[:accuracy]
    end
  end

  describe "insert/5" do
    setup %{user: user} do
      result = Location.insert(
        user,
        "testing",
        Address.latitude,
        Address.longitude,
        10
      )

      {:ok, result: result}
    end

    test "should return a successful result", %{result: result} do
      assert {:ok, %Location{}} = result
    end
  end

  describe "check_for_bot_events/1 with a user inside a bot perimeter" do
    setup %{user: user, bot: bot} do
      loc = %Location{
        user: user,
        lat: Bot.lat(bot),
        lon: Bot.lon(bot),
        accuracy: 10
      }

      {:ok, inside_loc: loc}
    end

    test "the bot owner should not generate an event", shared do
      Location.check_for_bot_events(shared.inside_loc, shared.owner)

      assert BotEvent.get_last_event(shared.owner.id, shared.bot.id) == nil
      assert Sandbox.list_notifications == []
    end

    test "bots with a negative radius should not generate an event", shared do
      shared.bot |> cast(%{radius: -1}, [:radius]) |> Repo.update!

      Location.check_for_bot_events(shared.inside_loc, shared.user)

      assert BotEvent.get_last_event(shared.user.id, shared.bot.id) == nil
      assert Sandbox.list_notifications == []
    end

    test "user who was outside should generate an event", shared do
      Location.check_for_bot_events(shared.inside_loc, shared.user)

      event = BotEvent.get_last_event_type(shared.user.id, shared.bot.id)
      assert event == :enter

      notifications = Sandbox.wait_notifications(count: 1, timeout: 5000)
      assert Enum.count(notifications) == 1
    end

    test "user who was already inside should not generate an event", shared do
      initial_event = BotEvent.insert(shared.user, shared.bot, :enter)
      Location.check_for_bot_events(shared.inside_loc, shared.user)

      event = BotEvent.get_last_event(shared.user.id, shared.bot.id)
      assert event.id == initial_event.id

      assert Sandbox.list_notifications == []
    end
  end

  describe "check_for_bot_events/1 with a user outside a bot perimeter" do
    setup %{user: user} do
      loc = Factory.build(:location, %{user: user})
      {:ok, outside_loc: loc}
    end

    test "user who was inside should generate an event", shared do
      BotEvent.insert(shared.user, shared.bot, :enter)
      Location.check_for_bot_events(shared.outside_loc, shared.user)

      event = BotEvent.get_last_event_type(shared.user.id, shared.bot.id)
      assert event == :exit

      notifications = Sandbox.wait_notifications(count: 1, timeout: 5000)
      assert Enum.count(notifications) == 1
    end

    test "user who was already outside should not generate an event", shared do
      initial_event = BotEvent.insert(shared.user, shared.bot, :exit)
      Location.check_for_bot_events(shared.outside_loc, shared.user)

      event = BotEvent.get_last_event(shared.user.id, shared.bot.id)
      assert event.id == initial_event.id

      assert Sandbox.list_notifications == []
    end

    test "unknown previous location should not generate an event", shared do
      Location.check_for_bot_events(shared.outside_loc, shared.user)

      assert BotEvent.get_last_event(shared.user.id, shared.bot.id) == nil
      assert Sandbox.list_notifications == []
    end
  end

  describe "update_bot_locations/1" do
    setup do
      loc = Factory.build(:location)
      {:ok, loc: loc}
    end

    test "unexpired follow me should update the bot location", shared do
      expiry = Timex.add(DateTime.utc_now, Duration.from_days(1))
      shared.bot
      |> Bot.changeset(%{follow_me: true, follow_me_expiry: expiry})
      |> Repo.update!

      Location.update_bot_locations(shared.loc, shared.owner)

      bot = Repo.get(Bot, shared.bot.id)
      assert Bot.lat(bot) == shared.loc.lat
      assert Bot.lon(bot) == shared.loc.lon
    end

    test "expired follow me should not update the bot location", shared do
      expiry = Timex.subtract(DateTime.utc_now, Duration.from_days(1))
      shared.bot
      |> Bot.changeset(%{follow_me: true, follow_me_expiry: expiry})
      |> Repo.update!

      Location.update_bot_locations(shared.loc, shared.owner)

      bot = Repo.get(Bot, shared.bot.id)
      refute Bot.lat(bot) == shared.loc.lat
      refute Bot.lon(bot) == shared.loc.lon
    end
  end
end
