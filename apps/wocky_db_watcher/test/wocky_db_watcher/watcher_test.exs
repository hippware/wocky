defmodule WockyDBWatcher.WatcherTest do
  defmodule TestConsumer do
    use GenStage

    alias Wocky.Bot
    alias Wocky.User
    alias WockyDBWatcher.Watcher

    @actions [:insert, :update, :delete]

    def start_link, do: GenStage.start_link(__MODULE__, nil, name: __MODULE__)

    def init(_) do
      subscriptions = Enum.map(@actions, &Watcher.name(Bot, &1, "test"))
      {:consumer, {nil, []}, subscribe_to: subscriptions}
    end

    def handle_events(events, _from, {nil, events_buf}) do
      {:noreply, [], {nil, events_buf ++ events}}
    end

    def handle_events(events, _from, {waiter, []}) do
      GenStage.reply(waiter, events)
      {:noreply, [], {nil, []}}
    end

    def handle_call(:get_events, from, {nil, []}),
      do: {:noreply, [], {from, []}}

    def handle_call(:get_events, _, {nil, events}),
      do: {:reply, events, [], {nil, []}}

    def handle_call(:has_events, _, {_, events} = state),
      do: {:reply, events != [], [], state}

    def get_events, do: GenStage.call(__MODULE__, :get_events, 1000)
    def has_events, do: GenStage.call(__MODULE__, :has_events)

    def actions, do: @actions
  end

  use ExUnit.Case, async: false

  alias Faker.Lorem
  alias Wocky.Bot
  alias Wocky.GeoUtils
  alias Wocky.Repo
  alias Wocky.Repo.Factory
  alias Wocky.User
  alias WockyDBWatcher.Watcher

  @rounding_error 1.0e-12

  setup_all do
    Enum.map(TestConsumer.actions(), fn action ->
      {:ok, _} = WockyDBWatcher.start_watcher(Bot, action, "test")
    end)

    # Because we can't use the Sandbox in its :manual mode (because it doesn't
    # cause the NOTIFY actions in the DB to fire) we have to do our own cleanup
    on_exit(fn ->
      Repo.delete_all(Bot)
      Repo.delete_all(User)
    end)
  end

  describe "insert action" do
    test "generates insert event" do
      TestConsumer.start_link()
      Factory.insert(:bot)
      [event] = TestConsumer.get_events()
      assert event.action == :insert
      assert event.object == Bot
      assert event.old == nil
      check_match(event.new, Bot.get(event.new.id))
      assert not TestConsumer.has_events()
    end

    test "generates insert event even with missing geometry" do
      TestConsumer.start_link()
      u = Factory.insert(:user)
      Bot.preallocate(u.id, u.server)
      [event] = TestConsumer.get_events()
      assert event.action == :insert
      assert event.object == Bot
      assert event.old == nil
      check_match(event.new, Bot.get(event.new.id, true))
      assert not TestConsumer.has_events()
    end

    test "provides whole integer locations as floats" do
      TestConsumer.start_link()
      Factory.insert(:bot, location: GeoUtils.point(5, 10))
      [event] = TestConsumer.get_events()
      check_match(event.new, Bot.get(event.new.id))
    end
  end

  describe "update action" do
    test "generates update event" do
      TestConsumer.start_link()
      original_bot = Bot.get(Factory.insert(:bot).id)
      _ = TestConsumer.get_events()

      Bot.update(original_bot, %{title: Lorem.sentence()})
      [event] = TestConsumer.get_events()
      assert event.action == :update
      assert event.object == Bot
      check_match(event.new, Bot.get(event.new.id))
      check_match(event.old, original_bot)
      assert not TestConsumer.has_events()
    end
  end

  describe "delete action" do
    test "generates delete event" do
      TestConsumer.start_link()
      original_bot = Bot.get(Factory.insert(:bot).id)
      _ = TestConsumer.get_events()

      Bot.delete(original_bot)
      [event] = TestConsumer.get_events()
      assert event.action == :delete
      assert event.object == Bot
      assert event.new == nil
      check_match(event.old, original_bot)
      assert not TestConsumer.has_events()
    end
  end

  defp check_match(a, b) do
    assert clean(a) == clean(b)

    if a.location && b.location do
      assert a.location.srid == b.location.srid

      # The JSON encoding by PostGIS is not quite as precise as the
      # data directly from the DB, so accept a small rounding error:
      {a_lon, a_lat} = a.location.coordinates
      {b_lon, b_lat} = b.location.coordinates
      Enum.each([a_lat, a_lon, b_lat, b_lon], fn x -> assert is_float(x) end)
      assert abs(a_lat - b_lat) < @rounding_error
      assert abs(a_lon - b_lon) < @rounding_error
    else
      assert a.location == b.location
    end

    # The timestamps can end up encoded with one digit less of precision as far
    # as DateTime is concerned, so using == to compare them can sometimes fail
    assert DateTime.compare(a.created_at, b.created_at) == :eq
    assert DateTime.compare(a.updated_at, b.updated_at) == :eq
  end

  defp clean(bot) do
    bot
    |> Map.put(:__meta__, Map.drop(bot.__meta__, [:state]))
    |> Map.drop([:location, :created_at, :updated_at])
  end
end
