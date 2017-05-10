defmodule Wocky.User.LocationSpec do
  use ESpec
  use ModelHelpers
  use Wocky.JID

  alias Faker.Address
  alias Wocky.Bot
  alias Wocky.Bot.Subscription
  alias Wocky.Events.BotPerimeterEvent
  alias Wocky.Repo.Timestamp
  alias Wocky.User.BotEvent
  alias Wocky.User.Location

  before_all do
    TestEventHandler.init
    Application.put_env(:wocky, :event_handler, TestEventHandler)
  end

  before do
    TestEventHandler.reset

    owner = Factory.insert(:user)
    user = Factory.insert(:user)

    bot_list = Factory.insert_list(3, :bot, user: owner)
    bot = hd(bot_list)

    :ok = Subscription.put(user, bot)

    {:ok, owner: owner, user: user, bot: bot, bot_list: bot_list}
  end

  describe "changeset/2 validations" do
    it "should pass with valid attributes" do
      data = %{resource: "testing", lat: 1.0, lon: 1.0, accuracy: 10}

      %Location{user: shared.user}
      |> Location.changeset(data)
      |> should(be_valid())
    end

    it "should fail if fields are missing" do
      %Location{user: shared.user}
      |> Location.changeset(%{})
      |> should(have_errors([:resource, :lat, :lon, :accuracy]))
    end

    it "should fail if the accuracy is negative" do
      data = %{resource: "testing", lat: 1.0, lon: 1.0, accuracy: -1}

      %Location{user: shared.user}
      |> Location.changeset(data)
      |> should(have_errors([:accuracy]))
    end
  end

  describe "insert/5" do
    before do
      result = Location.insert(
        shared.user,
        "testing",
        Address.latitude,
        Address.longitude,
        10
      )

      {:ok, result: result}
    end

    it "should return a successful result" do
      shared.result |> should(be_ok_result())
    end
  end

  describe "check_for_bot_events/1" do
    context "with a user location that is inside a bot perimeter" do
      before do
        loc = %Location{
          user: shared.user,
          lat: shared.bot.lat,
          lon: shared.bot.lon,
          accuracy: 10
        }

        {:ok, inside_loc: loc}
      end

      context "when the user owns the bot" do
        before do
          loc = %Location{shared.inside_loc | user: shared.owner}
          Location.check_for_bot_events(loc)
        end

        it "should not store an enter event" do
          event = BotEvent.get_last_event(shared.owner.id, shared.bot.id)
          event |> should(be_nil())
        end

        it "should not generate a notification" do
          TestEventHandler.get_events |> should(be_empty())
        end
      end

      context "when the bot has a negative radius" do
        before do
          shared.bot
          |> Bot.changeset(%{radius: -1})
          |> Repo.update!

          Location.check_for_bot_events(shared.inside_loc)
        end

        it "should not store an enter event" do
          event = BotEvent.get_last_event(shared.user.id, shared.bot.id)
          event |> should(be_nil())
        end

        it "should not generate a notification" do
          TestEventHandler.get_events |> should(be_empty())
        end
      end

      context "when there are no existing enter events" do
        before do
          Location.check_for_bot_events(shared.inside_loc)
        end

        it "should store an enter event" do
          shared.user.id
          |> BotEvent.get_last_event_type(shared.bot.id)
          |> should(eq :enter)
        end

        it "should generate a notification" do
          [{_, %BotPerimeterEvent{} = event}] = TestEventHandler.get_events

          event.user.id |> should(eq shared.user.id)
          event.bot.id |> should(eq shared.bot.id)
          event.event |> should(eq :enter)
        end
      end

      context "when there is already an existing enter event" do
        before do
          event = BotEvent.insert(shared.user, shared.bot, :enter)
          Location.check_for_bot_events(shared.inside_loc)
          {:ok, event: event}
        end

        it "should not store an enter event" do
          event = BotEvent.get_last_event(shared.user.id, shared.bot.id)
          event.id |> should(eq shared.event.id)
        end

        it "should not generate a notification" do
          TestEventHandler.get_events |> should(be_empty())
        end
      end
    end

    context "with a user location that is outside a bot perimeter" do
      before do
        loc = Factory.build(:location, %{user: shared.user})
        {:shared, outside_loc: loc}
      end

      context "when there is already an existing enter event" do
        before do
          event = BotEvent.insert(shared.user, shared.bot, :enter)
          Location.check_for_bot_events(shared.outside_loc)
          {:ok, event: event}
        end

        it "should store an exit event" do
          shared.user.id
          |> BotEvent.get_last_event_type(shared.bot.id)
          |> should(eq :exit)
        end

        it "should generate a notification" do
          [{_, %BotPerimeterEvent{} = event}] = TestEventHandler.get_events

          event.user.id |> should(eq shared.user.id)
          event.bot.id |> should(eq shared.bot.id)
          event.event |> should(eq :exit)
        end
      end

      context "when there is already an existing exit event" do
        before do
          event = BotEvent.insert(shared.user, shared.bot, :exit)
          Location.check_for_bot_events(shared.outside_loc)
          {:ok, event: event}
        end

        it "should not store an exit event" do
          event = BotEvent.get_last_event(shared.user.id, shared.bot.id)
          event.id |> should(eq shared.event.id)
        end

        it "should not generate a notification" do
          TestEventHandler.get_events |> should(be_empty())
        end
      end

      context "when there are no events" do
        before do
          Location.check_for_bot_events(shared.outside_loc)
        end

        it "should not store an exit event" do
          event = BotEvent.get_last_event(shared.user.id, shared.bot.id)
          event |> should(be_nil())
        end

        it "should not generate a notification" do
          TestEventHandler.get_events |> should(be_empty())
        end
      end
    end
  end

  describe "update_bot_locations/1" do
    context "with a user that has a bot set to 'follow me'" do
      before do
        loc = Factory.build(:location, %{user: shared.owner})
        {:shared, loc: loc}
      end

      context "and an expiry in the future" do
        before do
          expiry = Timestamp.now + 86400
          shared.bot
          |> Bot.changeset(%{follow_me: true, follow_me_expiry: expiry})
          |> Repo.update!

          Location.update_bot_locations(shared.loc)
        end

        it "should update the bot location" do
          bot = Repo.get(Bot, shared.bot.id)
          bot.lat |> should(eq shared.loc.lat)
          bot.lon |> should(eq shared.loc.lon)
        end
      end

      context "and an expiry in the past" do
        before do
          expiry = Timestamp.now - 86400
          shared.bot
          |> Bot.changeset(%{follow_me: true, follow_me_expiry: expiry})
          |> Repo.update!

          Location.update_bot_locations(shared.loc)
        end

        it "should not update the bot location" do
          bot = Repo.get(Bot, shared.bot.id)
          bot.lat |> should_not(eq shared.loc.lat)
          bot.lon |> should_not(eq shared.loc.lon)
        end
      end
    end
  end
end
