defmodule Wocky.Wachter.ClientTest do
  use ExUnit.Case, async: false

  alias Ecto.Changeset
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

    def start_link, do: GenServer.start_link(__MODULE__, nil, name: __MODULE__)

    def send_event(event, pid) do
      GenServer.call(__MODULE__, {:event, event, pid})
    end

    def init(_), do: {:ok, nil}

    def handle_call({:event, event, pid}, _from, state) do
      send(pid, event)
      {:reply, :ok, state}
    end
  end

  setup_all do
    Ecto.Adapters.SQL.Sandbox.mode(Wocky.Repo, :auto)
    # Give any DB notifications still in the system from previous tests
    # a grace period to finish up before we start the watcher
    :timer.sleep(500)
    Application.start(:wocky_db_watcher)
    Client.start_link()

    # Because we can't use the Sandbox in its :manual mode (because it doesn't
    # cause the NOTIFY actions in the DB to fire) we have to do our own cleanup
    on_exit(fn ->
      Application.stop(:wocky_db_watcher)
      Repo.delete_all(User)
    end)
  end

  setup do
    {:ok, cb} = Callback.start_link()

    on_exit(fn ->
      await_end(cb)
      Client.clear_all_subscriptions()
    end)
  end

  test "generates insert event" do
    self = self()
    Client.subscribe(Bot, :insert, &Callback.send_event(&1, self))
    %{id: bid} = Factory.insert(:bot)

    assert_receive %Event{action: :insert, old: nil, new: %Bot{id: ^bid}}, 300
  end

  test "generates update event" do
    self = self()
    Client.subscribe(Bot, :update, &Callback.send_event(&1, self))
    bot = %{id: bid} = Factory.insert(:bot)

    bot
    |> Changeset.cast(%{title: Lorem.sentence()}, [:title])
    |> Repo.update()

    assert_receive %Event{
                     action: :update,
                     old: %Bot{id: ^bid, title: title},
                     new: %Bot{id: ^bid, title: title2}
                   }
                   when title != title2,
                   300
  end

  test "generates delete event" do
    self = self()
    Client.subscribe(Bot, :delete, &Callback.send_event(&1, self))
    bot = %{id: bid} = Factory.insert(:bot)
    Repo.delete(bot)

    assert_receive %Event{action: :delete, new: nil, old: %Bot{id: ^bid}}, 300
  end

  test "unsubscribe" do
    {:ok, ref} =
      Client.subscribe(Bot, :insert, &Callback.send_event(&1, self()))

    Client.unsubscribe(ref)
    Factory.insert(:bot)
    refute_receive _, 200
  end

  test "crash handler" do
    client = Process.whereis(Client)
    {:ok, _ref} = Client.subscribe(Bot, :insert, &crash(&1))

    Factory.insert(:bot)

    :timer.sleep(500)
    client2 = Process.whereis(Client)
    assert client == client2
    assert Process.alive?(client)
  end

  defp crash(_event) do
    :lists.last([])
  end

  defp await_end(pid) do
    if Process.alive?(pid) do
      await_end(pid)
    else
      :ok
    end
  end
end
