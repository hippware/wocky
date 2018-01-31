defmodule Wocky.Wachter.ClientTest do
  use ExUnit.Case, async: false
  use Wocky.Repo.Model

  alias Faker.Lorem
  alias Wocky.Bot
  alias Wocky.Repo
  alias Wocky.Repo.Factory
  alias Wocky.User
  alias Wocky.Watcher.Client
  alias WockyDBWatcher.Event

  defmodule Callback do
    @moduledoc "Simple GenServer to collect and return events"
    use GenServer

    def start_link, do: GenServer.start_link(__MODULE__, [], name: __MODULE__)
    def send_event(event), do: GenServer.call(__MODULE__, {:event, event})
    def get_events, do: GenServer.call(__MODULE__, :get_events)

    def init(_), do: {:ok, []}
    def handle_call({:event, event}, _from, s), do: {:reply, :ok, [event | s]}
    def handle_call(:get_events, _from, s), do: {:reply, Enum.reverse(s), []}
  end

  setup_all do
    Ecto.Adapters.SQL.Sandbox.mode(Wocky.Repo, :auto)
    :timer.sleep(500)
    Application.start(:wocky_db_watcher)

    # Because we can't use the Sandbox in its :manual mode (because it doesn't
    # cause the NOTIFY actions in the DB to fire) we have to do our own cleanup
    on_exit(fn ->
      Application.stop(:wocky_db_watcher)
      Repo.delete_all(User)
    end)
  end

  setup do
    {:ok, cb} = Callback.start_link()
    {:ok, cn} = Client.start_link()

    on_exit(fn ->
      await_end(cb)
      await_end(cn)
    end)
  end

  test "generates insert event" do
    Client.subscribe(Bot, :insert, &Callback.send_event/1)
    bot = Factory.insert(:bot)
    :timer.sleep(200)
    [event] = Callback.get_events()
    assert %Event{action: :insert, object: Bot, old: nil} = event
    assert bot.id == event.new.id
  end

  test "generates update event" do
    Client.subscribe(Bot, :update, &Callback.send_event/1)
    bot = Factory.insert(:bot)
    bot
    |> cast(%{title: Lorem.sentence()}, [:title])
    |> Repo.update()
    :timer.sleep(200)
    [event] = Callback.get_events()
    assert %Event{action: :update, object: Bot} = event
    assert bot.id == event.old.id
    assert bot.id == event.new.id
    assert event.old.title != event.new.title
  end

  test "generates delete event" do
    Client.subscribe(Bot, :delete, &Callback.send_event/1)
    bot = Factory.insert(:bot)
    Repo.delete(bot)
    :timer.sleep(200)
    [event] = Callback.get_events()
    assert %Event{action: :delete, object: Bot, new: nil} = event
    assert bot.id == event.old.id
  end

  test "unsubscribe" do
    {:ok, ref} = Client.subscribe(Bot, :insert, &Callback.send_event/1)
    Client.unsubscribe(ref)
    Factory.insert(:bot)
    :timer.sleep(200)
    assert [] = Callback.get_events()
  end

  defp await_end(pid) do
    if Process.alive?(pid) do
      await_end(pid)
    else
      :ok
    end
  end
end
