defmodule Wocky.User.GeoFenceTest do
  use Wocky.DataCase

  import Ecto.Query

  alias Faker.Code
  alias Wocky.Bot
  alias Wocky.Push
  alias Wocky.Push.Sandbox
  alias Wocky.Repo
  alias Wocky.Repo.Factory
  alias Wocky.User
  alias Wocky.User.{BotEvent, Location, GeoFence}

  @rsrc "testing"

  setup_all do
    Application.put_env(:wocky, :visit_timeout_seconds, 2)
  end

  setup do
    Sandbox.clear_notifications()

    owner = Factory.insert(:user)
    Push.enable(owner.id, @rsrc, Code.isbn13())

    user = Factory.insert(:user)
    Push.enable(user.id, @rsrc, Code.isbn13())

    # This user should never get notified in spite of being a guest
    hidden_user = Factory.insert(:user)
    User.hide(hidden_user, true)
    Push.enable(hidden_user.id, @rsrc, Code.isbn13())

    bot_list = Factory.insert_list(3, :bot, user: owner, geofence: true)
    bot = hd(bot_list)

    :ok = Bot.subscribe(bot, user, true)
    :ok = Bot.subscribe(bot, hidden_user, true)

    {:ok, owner: owner, user: user, bot: bot, bot_list: bot_list}
  end

  defp insert_offset_bot_event(user, bot, event, offset) do
    event = BotEvent.insert(user, @rsrc, bot, event)
    timestamp = Timex.shift(Timex.now(), seconds: offset)

    from(be in BotEvent, where: be.id == ^event.id)
    |> Repo.update_all(set: [created_at: timestamp])
  end

  defp visit_bot(bot, user) do
    Bot.visit(bot, user)
    Sandbox.wait_notifications(count: 1, timeout: 500)
    Sandbox.clear_notifications()
  end

  describe "check_for_bot_events/1 with a user inside a bot perimeter" do
    setup %{user: user, bot: bot} do
      loc = %Location{
        user: user,
        lat: Bot.lat(bot),
        lon: Bot.lon(bot),
        accuracy: 10,
        resource: @rsrc
      }

      {:ok, inside_loc: loc}
    end

    test "bots with a negative radius should not generate an event", ctx do
      ctx.bot |> cast(%{radius: -1}, [:radius]) |> Repo.update!()

      GeoFence.check_for_bot_events(ctx.inside_loc, ctx.user)

      assert BotEvent.get_last_event(ctx.user.id, @rsrc, ctx.bot.id) == nil
      assert Sandbox.list_notifications() == []
    end

    test "with no bot perimeter events", ctx do
      GeoFence.check_for_bot_events(ctx.inside_loc, ctx.user)

      event = BotEvent.get_last_event_type(ctx.user.id, @rsrc, ctx.bot.id)
      assert event == :transition_in

      assert Bot.subscription(ctx.bot, ctx.user) == :guest

      assert Sandbox.list_notifications() == []
    end

    test "who was outside the bot perimeter", ctx do
      BotEvent.insert(ctx.user, @rsrc, ctx.bot, :exit)
      GeoFence.check_for_bot_events(ctx.inside_loc, ctx.user)

      event = BotEvent.get_last_event_type(ctx.user.id, @rsrc, ctx.bot.id)
      assert event == :transition_in

      assert Bot.subscription(ctx.bot, ctx.user) == :guest

      assert Sandbox.list_notifications() == []
    end

    test "who was transitioning out of the bot perimeter", ctx do
      initial_event = BotEvent.insert(ctx.user, @rsrc, ctx.bot, :enter)
      to_event = BotEvent.insert(ctx.user, @rsrc, ctx.bot, :transition_out)

      GeoFence.check_for_bot_events(ctx.inside_loc, ctx.user)

      event = BotEvent.get_last_event(ctx.user.id, @rsrc, ctx.bot.id)
      refute event.id == to_event.id
      assert event.id == initial_event.id

      assert Bot.subscription(ctx.bot, ctx.user) == :guest

      assert Sandbox.list_notifications() == []
    end

    test "who was transitioning into the bot perimeter", ctx do
      initial_event = BotEvent.insert(ctx.user, @rsrc, ctx.bot, :transition_in)
      GeoFence.check_for_bot_events(ctx.inside_loc, ctx.user)

      event = BotEvent.get_last_event(ctx.user.id, @rsrc, ctx.bot.id)
      assert event.id == initial_event.id

      assert Bot.subscription(ctx.bot, ctx.user) == :guest

      assert Sandbox.list_notifications() == []
    end

    test "who has transitioned into the bot perimeter", ctx do
      insert_offset_bot_event(ctx.user, ctx.bot, :transition_in, -150)
      GeoFence.check_for_bot_events(ctx.inside_loc, ctx.user)

      event = BotEvent.get_last_event_type(ctx.user.id, @rsrc, ctx.bot.id)
      assert event == :enter

      assert Bot.subscription(ctx.bot, ctx.user) == :visitor

      notifications = Sandbox.wait_notifications(count: 1, timeout: 5000)
      assert Enum.count(notifications) == 1
    end

    test "who has timed out inside the bot permimeter", ctx do
      BotEvent.insert(ctx.user, @rsrc, ctx.bot, :timeout)
      GeoFence.check_for_bot_events(ctx.inside_loc, ctx.user)

      event = BotEvent.get_last_event_type(ctx.user.id, @rsrc, ctx.bot.id)
      assert event == :reactivate

      assert Bot.subscription(ctx.bot, ctx.user) == :visitor

      assert Sandbox.list_notifications() == []
    end

    test "who has reactivated inside the bot perimeter", ctx do
      visit_bot(ctx.bot, ctx.user)
      initial_event = BotEvent.insert(ctx.user, @rsrc, ctx.bot, :reactivate)
      GeoFence.check_for_bot_events(ctx.inside_loc, ctx.user)

      event = BotEvent.get_last_event(ctx.user.id, @rsrc, ctx.bot.id)
      assert event.id == initial_event.id

      assert Bot.subscription(ctx.bot, ctx.user) == :visitor

      assert Sandbox.list_notifications() == []
    end

    test "who has reactivated outside the bot perimeter", ctx do
      BotEvent.insert(ctx.user, @rsrc, ctx.bot, :deactivate)
      GeoFence.check_for_bot_events(ctx.inside_loc, ctx.user)

      event = BotEvent.get_last_event_type(ctx.user.id, @rsrc, ctx.bot.id)
      assert event == :transition_in

      assert Bot.subscription(ctx.bot, ctx.user) == :guest

      assert Sandbox.list_notifications() == []
    end

    test "who was already inside the bot perimeter", ctx do
      visit_bot(ctx.bot, ctx.user)
      initial_event = BotEvent.insert(ctx.user, @rsrc, ctx.bot, :enter)
      GeoFence.check_for_bot_events(ctx.inside_loc, ctx.user)

      event = BotEvent.get_last_event(ctx.user.id, @rsrc, ctx.bot.id)
      assert event.id == initial_event.id

      assert Bot.subscription(ctx.bot, ctx.user) == :visitor

      assert Sandbox.list_notifications() == []
    end
  end

  describe """
  check_for_bot_events/1 with a user inside a bot perimeter - no debounce
  """ do
    setup %{user: user, bot: bot} do
      loc = %Location{
        user: user,
        lat: Bot.lat(bot),
        lon: Bot.lon(bot),
        accuracy: 10,
        resource: @rsrc
      }

      {:ok, inside_loc: loc}
    end

    test "bots with a negative radius should not generate an event", ctx do
      bot = ctx.bot |> cast(%{radius: -1}, [:radius]) |> Repo.update!()

      GeoFence.check_for_bot_event(bot, ctx.inside_loc, ctx.user)

      assert BotEvent.get_last_event(ctx.user.id, @rsrc, ctx.bot.id) == nil
      assert Sandbox.list_notifications() == []
    end

    test "with no bot perimeter events", ctx do
      GeoFence.check_for_bot_event(ctx.bot, ctx.inside_loc, ctx.user)

      event = BotEvent.get_last_event_type(ctx.user.id, @rsrc, ctx.bot.id)
      assert event == :enter

      assert Bot.subscription(ctx.bot, ctx.user) == :visitor

      notifications = Sandbox.wait_notifications(count: 1, timeout: 5000)
      assert Enum.count(notifications) == 1
    end

    test "who was outside the bot perimeter", ctx do
      BotEvent.insert(ctx.user, @rsrc, ctx.bot, :exit)
      GeoFence.check_for_bot_event(ctx.bot, ctx.inside_loc, ctx.user)

      event = BotEvent.get_last_event_type(ctx.user.id, @rsrc, ctx.bot.id)
      assert event == :enter

      assert Bot.subscription(ctx.bot, ctx.user) == :visitor

      notifications = Sandbox.wait_notifications(count: 1, timeout: 5000)
      assert Enum.count(notifications) == 1
    end

    test "who was transitioning out of the bot perimeter", ctx do
      initial_event = BotEvent.insert(ctx.user, @rsrc, ctx.bot, :enter)
      to_event = BotEvent.insert(ctx.user, @rsrc, ctx.bot, :transition_out)

      GeoFence.check_for_bot_event(ctx.bot, ctx.inside_loc, ctx.user)

      event = BotEvent.get_last_event(ctx.user.id, @rsrc, ctx.bot.id)
      refute event.id == to_event.id
      assert event.id == initial_event.id

      assert Bot.subscription(ctx.bot, ctx.user) == :guest

      assert Sandbox.list_notifications() == []
    end

    test "who was transitioning into the bot perimeter", ctx do
      BotEvent.insert(ctx.user, @rsrc, ctx.bot, :transition_in)
      GeoFence.check_for_bot_event(ctx.bot, ctx.inside_loc, ctx.user)

      event = BotEvent.get_last_event_type(ctx.user.id, @rsrc, ctx.bot.id)
      assert event == :enter

      assert Bot.subscription(ctx.bot, ctx.user) == :visitor

      notifications = Sandbox.wait_notifications(count: 1, timeout: 5000)
      assert Enum.count(notifications) == 1
    end

    test "who has timed out inside the bot permimeter", ctx do
      BotEvent.insert(ctx.user, @rsrc, ctx.bot, :timeout)
      GeoFence.check_for_bot_event(ctx.bot, ctx.inside_loc, ctx.user)

      event = BotEvent.get_last_event_type(ctx.user.id, @rsrc, ctx.bot.id)
      assert event == :reactivate

      assert Bot.subscription(ctx.bot, ctx.user) == :visitor

      assert Sandbox.list_notifications() == []
    end

    test "who has reactivated inside the bot perimeter", ctx do
      visit_bot(ctx.bot, ctx.user)
      initial_event = BotEvent.insert(ctx.user, @rsrc, ctx.bot, :reactivate)
      GeoFence.check_for_bot_event(ctx.bot, ctx.inside_loc, ctx.user)

      event = BotEvent.get_last_event(ctx.user.id, @rsrc, ctx.bot.id)
      assert event.id == initial_event.id

      assert Bot.subscription(ctx.bot, ctx.user) == :visitor

      assert Sandbox.list_notifications() == []
    end

    test "who has reactivated outside the bot perimeter", ctx do
      BotEvent.insert(ctx.user, @rsrc, ctx.bot, :deactivate)
      GeoFence.check_for_bot_event(ctx.bot, ctx.inside_loc, ctx.user)

      event = BotEvent.get_last_event_type(ctx.user.id, @rsrc, ctx.bot.id)
      assert event == :enter

      assert Bot.subscription(ctx.bot, ctx.user) == :visitor

      notifications = Sandbox.wait_notifications(count: 1, timeout: 5000)
      assert Enum.count(notifications) == 1
    end

    test "who was already inside the bot perimeter", ctx do
      visit_bot(ctx.bot, ctx.user)
      initial_event = BotEvent.insert(ctx.user, @rsrc, ctx.bot, :enter)
      GeoFence.check_for_bot_event(ctx.bot, ctx.inside_loc, ctx.user)

      event = BotEvent.get_last_event(ctx.user.id, @rsrc, ctx.bot.id)
      assert event.id == initial_event.id

      assert Bot.subscription(ctx.bot, ctx.user) == :visitor

      assert Sandbox.list_notifications() == []
    end
  end

  describe "check_for_bot_events/1 with a user outside a bot perimeter" do
    setup %{user: user} do
      loc = Factory.build(:location, %{user: user, resource: @rsrc})
      {:ok, outside_loc: loc}
    end

    test "with no bot perimeter events", ctx do
      GeoFence.check_for_bot_events(ctx.outside_loc, ctx.user)

      event = BotEvent.get_last_event(ctx.user.id, @rsrc, ctx.bot.id)
      assert event == nil

      assert Bot.subscription(ctx.bot, ctx.user) == :guest

      assert Sandbox.list_notifications() == []
    end

    test "who was inside the bot perimeter", ctx do
      visit_bot(ctx.bot, ctx.user)
      BotEvent.insert(ctx.user, @rsrc, ctx.bot, :enter)
      GeoFence.check_for_bot_events(ctx.outside_loc, ctx.user)

      event = BotEvent.get_last_event_type(ctx.user.id, @rsrc, ctx.bot.id)
      assert event == :transition_out

      assert Bot.subscription(ctx.bot, ctx.user) == :visitor

      assert Sandbox.list_notifications() == []
    end

    test "who was transitioning into the the bot perimeter", ctx do
      initial_event = BotEvent.insert(ctx.user, @rsrc, ctx.bot, :exit)
      to_event = BotEvent.insert(ctx.user, @rsrc, ctx.bot, :transition_in)
      GeoFence.check_for_bot_events(ctx.outside_loc, ctx.user)

      event = BotEvent.get_last_event(ctx.user.id, @rsrc, ctx.bot.id)
      refute event.id == to_event.id
      assert event.id == initial_event.id

      assert Bot.subscription(ctx.bot, ctx.user) == :guest

      assert Sandbox.list_notifications() == []
    end

    test "who was transitioning out of the bot perimeter", ctx do
      visit_bot(ctx.bot, ctx.user)
      initial_event = BotEvent.insert(ctx.user, @rsrc, ctx.bot, :transition_out)
      GeoFence.check_for_bot_events(ctx.outside_loc, ctx.user)

      event = BotEvent.get_last_event(ctx.user.id, @rsrc, ctx.bot.id)
      assert event.id == initial_event.id

      assert Bot.subscription(ctx.bot, ctx.user) == :visitor

      assert Sandbox.list_notifications() == []
    end

    test "who has transitioned out of the bot perimeter", ctx do
      visit_bot(ctx.bot, ctx.user)
      insert_offset_bot_event(ctx.user, ctx.bot, :transition_out, -80)
      GeoFence.check_for_bot_events(ctx.outside_loc, ctx.user)

      event = BotEvent.get_last_event_type(ctx.user.id, @rsrc, ctx.bot.id)
      assert event == :exit

      assert Bot.subscription(ctx.bot, ctx.user) == :guest

      notifications = Sandbox.wait_notifications(count: 1, timeout: 5000)
      assert Enum.count(notifications) == 1
    end

    test "who has timed out inside the bot permimeter", ctx do
      BotEvent.insert(ctx.user, @rsrc, ctx.bot, :timeout)
      GeoFence.check_for_bot_events(ctx.outside_loc, ctx.user)

      event = BotEvent.get_last_event_type(ctx.user.id, @rsrc, ctx.bot.id)
      assert event == :deactivate

      assert Bot.subscription(ctx.bot, ctx.user) == :guest

      assert Sandbox.list_notifications() == []
    end

    test "who has reactivated inside the bot perimeter", ctx do
      visit_bot(ctx.bot, ctx.user)
      BotEvent.insert(ctx.user, @rsrc, ctx.bot, :reactivate)
      GeoFence.check_for_bot_events(ctx.outside_loc, ctx.user)

      event = BotEvent.get_last_event_type(ctx.user.id, @rsrc, ctx.bot.id)
      assert event == :transition_out

      assert Bot.subscription(ctx.bot, ctx.user) == :visitor

      assert Sandbox.list_notifications() == []
    end

    test "who has reactivated outside the bot perimeter", ctx do
      initial_event = BotEvent.insert(ctx.user, @rsrc, ctx.bot, :deactivate)
      GeoFence.check_for_bot_events(ctx.outside_loc, ctx.user)

      event = BotEvent.get_last_event(ctx.user.id, @rsrc, ctx.bot.id)
      assert event.id == initial_event.id

      assert Bot.subscription(ctx.bot, ctx.user) == :guest

      assert Sandbox.list_notifications() == []
    end

    test "who was already outside the bot perimeter", ctx do
      initial_event = BotEvent.insert(ctx.user, @rsrc, ctx.bot, :exit)
      GeoFence.check_for_bot_events(ctx.outside_loc, ctx.user)

      event = BotEvent.get_last_event(ctx.user.id, @rsrc, ctx.bot.id)
      assert event.id == initial_event.id

      assert Bot.subscription(ctx.bot, ctx.user) == :guest

      assert Sandbox.list_notifications() == []
    end
  end

  describe """
  check_for_bot_events/1 with a user outside a bot perimeter - no debounce
  """ do
    setup %{user: user} do
      loc = Factory.build(:location, %{user: user, resource: @rsrc})
      {:ok, outside_loc: loc}
    end

    test "with no bot perimeter events", ctx do
      GeoFence.check_for_bot_event(ctx.bot, ctx.outside_loc, ctx.user)

      event = BotEvent.get_last_event(ctx.user.id, @rsrc, ctx.bot.id)
      assert event == nil

      assert Bot.subscription(ctx.bot, ctx.user) == :guest

      assert Sandbox.list_notifications() == []
    end

    test "who was inside the bot perimeter", ctx do
      visit_bot(ctx.bot, ctx.user)
      BotEvent.insert(ctx.user, @rsrc, ctx.bot, :enter)
      GeoFence.check_for_bot_event(ctx.bot, ctx.outside_loc, ctx.user)

      event = BotEvent.get_last_event_type(ctx.user.id, @rsrc, ctx.bot.id)
      assert event == :exit

      assert Bot.subscription(ctx.bot, ctx.user) == :guest

      notifications = Sandbox.wait_notifications(count: 1, timeout: 5000)
      assert Enum.count(notifications) == 1
    end

    test "who was transitioning into the the bot perimeter", ctx do
      initial_event = BotEvent.insert(ctx.user, @rsrc, ctx.bot, :exit)
      to_event = BotEvent.insert(ctx.user, @rsrc, ctx.bot, :transition_in)
      GeoFence.check_for_bot_event(ctx.bot, ctx.outside_loc, ctx.user)

      event = BotEvent.get_last_event(ctx.user.id, @rsrc, ctx.bot.id)
      refute event.id == to_event.id
      assert event.id == initial_event.id

      assert Bot.subscription(ctx.bot, ctx.user) == :guest

      assert Sandbox.list_notifications() == []
    end

    test "who was transitioning out of the bot perimeter", ctx do
      visit_bot(ctx.bot, ctx.user)
      BotEvent.insert(ctx.user, @rsrc, ctx.bot, :transition_out)
      GeoFence.check_for_bot_event(ctx.bot, ctx.outside_loc, ctx.user)

      event = BotEvent.get_last_event_type(ctx.user.id, @rsrc, ctx.bot.id)
      assert event == :exit

      assert Bot.subscription(ctx.bot, ctx.user) == :guest

      notifications = Sandbox.wait_notifications(count: 1, timeout: 5000)
      assert Enum.count(notifications) == 1
    end

    test "who has timed out inside the bot permimeter", ctx do
      BotEvent.insert(ctx.user, @rsrc, ctx.bot, :timeout)
      GeoFence.check_for_bot_event(ctx.bot, ctx.outside_loc, ctx.user)

      event = BotEvent.get_last_event_type(ctx.user.id, @rsrc, ctx.bot.id)
      assert event == :deactivate

      assert Bot.subscription(ctx.bot, ctx.user) == :guest

      assert Sandbox.list_notifications() == []
    end

    test "who has reactivated inside the bot perimeter", ctx do
      visit_bot(ctx.bot, ctx.user)
      BotEvent.insert(ctx.user, @rsrc, ctx.bot, :reactivate)
      GeoFence.check_for_bot_event(ctx.bot, ctx.outside_loc, ctx.user)

      event = BotEvent.get_last_event_type(ctx.user.id, @rsrc, ctx.bot.id)
      assert event == :exit

      assert Bot.subscription(ctx.bot, ctx.user) == :guest

      notifications = Sandbox.wait_notifications(count: 1, timeout: 5000)
      assert Enum.count(notifications) == 1
    end

    test "who has reactivated outside the bot perimeter", ctx do
      initial_event = BotEvent.insert(ctx.user, @rsrc, ctx.bot, :deactivate)
      GeoFence.check_for_bot_event(ctx.bot, ctx.outside_loc, ctx.user)

      event = BotEvent.get_last_event(ctx.user.id, @rsrc, ctx.bot.id)
      assert event.id == initial_event.id

      assert Bot.subscription(ctx.bot, ctx.user) == :guest

      assert Sandbox.list_notifications() == []
    end

    test "who was already outside the bot perimeter", ctx do
      initial_event = BotEvent.insert(ctx.user, @rsrc, ctx.bot, :exit)
      GeoFence.check_for_bot_event(ctx.bot, ctx.outside_loc, ctx.user)

      event = BotEvent.get_last_event(ctx.user.id, @rsrc, ctx.bot.id)
      assert event.id == initial_event.id

      assert Bot.subscription(ctx.bot, ctx.user) == :guest

      assert Sandbox.list_notifications() == []
    end
  end

  describe "visitor timeout" do
    test "user should exit bot if no location is received in timeout", %{
      user: user,
      bot: bot
    } do
      visit_bot(bot, user)

      :ok = User.set_location(user, @rsrc, Bot.lat(bot), Bot.lon(bot), 10)

      Process.sleep(1000)
      assert Bot.subscription(bot, user) == :visitor

      Process.sleep(1500)
      assert Bot.subscription(bot, user) == :guest
    end

    test "user should remain visiting if they send subsequent updates", %{
      user: user,
      bot: bot
    } do
      visit_bot(bot, user)

      lat = Bot.lat(bot)
      lon = Bot.lon(bot)

      :ok = User.set_location(user, @rsrc, lat, lon, 10)

      Process.sleep(1000)
      assert Bot.subscription(bot, user) == :visitor

      :ok = User.set_location(user, @rsrc, lat, lon, 10)

      Process.sleep(1500)
      assert Bot.subscription(bot, user) == :visitor
    end

    test "user should not be exited early", %{user: user, bot: bot} do
      outside_loc = Factory.build(:location, %{user: user})

      :ok = User.set_location(user, @rsrc, outside_loc.lat, outside_loc.lon, 10)

      Process.sleep(1500)
      assert Bot.subscription(bot, user) == :guest

      visit_bot(bot, user)

      :ok = User.set_location(user, @rsrc, Bot.lat(bot), Bot.lon(bot), 10)

      Process.sleep(1000)
      assert Bot.subscription(bot, user) == :visitor

      Process.sleep(2000)
      assert Bot.subscription(bot, user) == :guest
    end
  end

  describe "exit_all_bots/1" do
    test "should un-visit all visited bots and send no notifiations", %{
      user: user,
      bot: bot
    } do
      visit_bot(bot, user)

      GeoFence.exit_all_bots(user)

      assert Bot.subscription(bot, user) == :guest

      assert Sandbox.list_notifications() == []
    end
  end
end
