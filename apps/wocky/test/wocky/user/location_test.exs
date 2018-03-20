defmodule Wocky.User.LocationTest do
  use Wocky.DataCase
  use Wocky.JID

  import Ecto.Query

  alias Faker.Address
  alias Faker.Code
  alias Wocky.Bot
  alias Wocky.Push
  alias Wocky.Push.Sandbox
  alias Wocky.Repo
  alias Wocky.Repo.Factory
  alias Wocky.User.BotEvent
  alias Wocky.User.Location

  @rsrc "testing"

  setup do
    Sandbox.clear_notifications()

    owner = Factory.insert(:user)
    Push.enable(owner.id, @rsrc, Code.isbn13())

    user = Factory.insert(:user)
    Push.enable(user.id, @rsrc, Code.isbn13())

    bot_list = Factory.insert_list(3, :bot, user: owner, geofence: true)
    bot = hd(bot_list)

    :ok = Bot.subscribe(bot, user, true)

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
      result =
        Location.insert(
          user,
          "testing",
          Address.latitude(),
          Address.longitude(),
          10
        )

      {:ok, result: result}
    end

    test "should return a successful result", %{result: result} do
      assert {:ok, %Location{}} = result
    end
  end

  defp insert_offset_bot_event(user, bot, event, offset) do
    event = BotEvent.insert(user, bot, event)
    timestamp = Timex.shift(Timex.now, seconds: offset)

    from(be in BotEvent, where: be.id == ^event.id)
    |> Repo.update_all(set: [created_at: timestamp])
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

    test "bots with a negative radius should not generate an event", shared do
      shared.bot |> cast(%{radius: -1}, [:radius]) |> Repo.update!()

      Location.check_for_bot_events(shared.inside_loc, shared.user)

      assert BotEvent.get_last_event(shared.user.id, shared.bot.id) == nil
      assert Sandbox.list_notifications() == []
    end

    test "with no bot perimeter events", shared do
      Location.check_for_bot_events(shared.inside_loc, shared.user)

      event = BotEvent.get_last_event_type(shared.user.id, shared.bot.id)
      assert event == :transition_in

      assert Bot.subscription(shared.bot, shared.user) == :guest

      assert Sandbox.list_notifications() == []
    end

    test "who was outside the bot perimeter", shared do
      BotEvent.insert(shared.user, shared.bot, :exit)
      Location.check_for_bot_events(shared.inside_loc, shared.user)

      event = BotEvent.get_last_event_type(shared.user.id, shared.bot.id)
      assert event == :transition_in

      assert Bot.subscription(shared.bot, shared.user) == :guest

      assert Sandbox.list_notifications() == []
    end

    test "who was transitioning out of the bot perimeter", shared do
      initial_event = BotEvent.insert(shared.user, shared.bot, :enter)
      to_event = BotEvent.insert(shared.user, shared.bot, :transition_out)
      Location.check_for_bot_events(shared.inside_loc, shared.user)

      event = BotEvent.get_last_event(shared.user.id, shared.bot.id)
      refute event.id == to_event.id
      assert event.id == initial_event.id

      assert Bot.subscription(shared.bot, shared.user) == :guest

      assert Sandbox.list_notifications() == []
    end

    test "who was transitioning into the bot perimeter", shared do
      initial_event = BotEvent.insert(shared.user, shared.bot, :transition_in)
      Location.check_for_bot_events(shared.inside_loc, shared.user)

      event = BotEvent.get_last_event(shared.user.id, shared.bot.id)
      assert event.id == initial_event.id

      assert Bot.subscription(shared.bot, shared.user) == :guest

      assert Sandbox.list_notifications() == []
    end

    test "who has transitioned into the bot perimeter", shared do
      insert_offset_bot_event(shared.user, shared.bot, :transition_in, -150)
      Location.check_for_bot_events(shared.inside_loc, shared.user)

      event = BotEvent.get_last_event_type(shared.user.id, shared.bot.id)
      assert event == :enter

      assert Bot.subscription(shared.bot, shared.user) == :visitor

     # notifications = Sandbox.wait_notifications(count: 1, timeout: 5000)
     # assert Enum.count(notifications) == 1
    end

    test "who was already inside the bot perimeter", shared do
      Bot.visit(shared.bot, shared.user)
      initial_event = BotEvent.insert(shared.user, shared.bot, :enter)
      Location.check_for_bot_events(shared.inside_loc, shared.user)

      event = BotEvent.get_last_event(shared.user.id, shared.bot.id)
      assert event.id == initial_event.id

      assert Bot.subscription(shared.bot, shared.user) == :visitor

      assert Sandbox.list_notifications() == []
    end
  end

  describe "check_for_bot_events/1 with a user outside a bot perimeter" do
    setup %{user: user} do
      loc = Factory.build(:location, %{user: user})
      {:ok, outside_loc: loc}
    end

    test "with no bot perimeter events", shared do
      Location.check_for_bot_events(shared.outside_loc, shared.user)

      event = BotEvent.get_last_event(shared.user.id, shared.bot.id)
      assert event == nil

      assert Bot.subscription(shared.bot, shared.user) == :guest

      assert Sandbox.list_notifications() == []
    end

    test "who was inside the bot perimeter", shared do
      Bot.visit(shared.user, shared.bot)
      BotEvent.insert(shared.user, shared.bot, :enter)
      Location.check_for_bot_events(shared.outside_loc, shared.user)

      event = BotEvent.get_last_event_type(shared.user.id, shared.bot.id)
      assert event == :transition_out

      assert Bot.subscription(shared.bot, shared.user) == :guest

      assert Sandbox.list_notifications() == []
    end

    test "who was transitioning into the the bot perimeter", shared do
      initial_event = BotEvent.insert(shared.user, shared.bot, :exit)
      to_event = BotEvent.insert(shared.user, shared.bot, :transition_in)
      Location.check_for_bot_events(shared.outside_loc, shared.user)

      event = BotEvent.get_last_event(shared.user.id, shared.bot.id)
      refute event.id == to_event.id
      assert event.id == initial_event.id

      assert Bot.subscription(shared.bot, shared.user) == :guest

      assert Sandbox.list_notifications() == []
    end

    test "who was transitioning out of the bot perimeter", shared do
      Bot.visit(shared.bot, shared.user)
      initial_event = BotEvent.insert(shared.user, shared.bot, :transition_out)
      Location.check_for_bot_events(shared.outside_loc, shared.user)

      event = BotEvent.get_last_event(shared.user.id, shared.bot.id)
      assert event.id == initial_event.id

      assert Bot.subscription(shared.bot, shared.user) == :visitor

      assert Sandbox.list_notifications() == []
    end

    test "who has transitioned out of the bot perimeter", shared do
      Bot.visit(shared.bot, shared.user)
      insert_offset_bot_event(shared.user, shared.bot, :transition_out, -80)
      Location.check_for_bot_events(shared.outside_loc, shared.user)

      event = BotEvent.get_last_event_type(shared.user.id, shared.bot.id)
      assert event == :exit

      assert Bot.subscription(shared.bot, shared.user) == :guest

      # notifications = Sandbox.wait_notifications(count: 1, timeout: 5000)
      # assert Enum.count(notifications) == 1
    end

    test "who was already outside the bot perimeter", shared do
      initial_event = BotEvent.insert(shared.user, shared.bot, :exit)
      Location.check_for_bot_events(shared.outside_loc, shared.user)

      event = BotEvent.get_last_event(shared.user.id, shared.bot.id)
      assert event.id == initial_event.id

      assert Bot.subscription(shared.bot, shared.user) == :guest

      assert Sandbox.list_notifications() == []
    end
  end
end
