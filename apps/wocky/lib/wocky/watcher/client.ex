defmodule Wocky.Watcher.Client do
  @moduledoc """
  The client for the wocky DB watcher - entities interested in DB callback
  events should subscribe to this process.
  """

  defmodule State do
    @moduledoc "State record for wocky watcher client"

    defstruct [
      :subscribers,
      :table_map
    ]
  end

  use GenServer

  alias Wocky.Watcher.EventDecoder
  alias Wocky.Watcher.Poller

  def start_link, do: GenServer.start_link(__MODULE__, nil, name: __MODULE__)

  def send(events), do: GenServer.cast(__MODULE__, {:send, events})

  def subscribe(object, action, fun) do
    GenServer.call(__MODULE__, {:subscribe, object, action, fun})
  end

  def unsubscribe(ref) do
    GenServer.call(__MODULE__, {:unsubscribe, ref})
  end

  def clear_all_subscriptions do
    GenServer.call(__MODULE__, :clear_all_subscriptions)
  end

  def init(_) do
    source =
      :wocky_db_watcher
      |> Confex.get_env(:backend, WockyDBWatcher.Backend.SQS)

    source.init
    Poller.start_link(source, __MODULE__)

    {:ok, %State{subscribers: %{}, table_map: Map.new()}}
  end

  def handle_cast({:send, events}, state) do
    Enum.each(events, &forward_event(&1, state))
    {:noreply, state}
  end

  def handle_call({:subscribe, object, action, fun}, _from, state) do
    current = Map.get(state.subscribers, {object, action}, MapSet.new())
    ref = make_ref()

    new_subscribers =
      Map.put(
        state.subscribers,
        {object, action},
        MapSet.put(current, {fun, ref})
      )

    new_table_map = Map.put(state.table_map, object.__schema__(:source), object)

    {:reply, {:ok, ref},
     %{state | subscribers: new_subscribers, table_map: new_table_map}}
  end

  def handle_call({:unsubscribe, ref}, _from, state) do
    new_subscribers =
      state.subscribers
      |> Enum.map(&delete_ref(&1, ref))
      |> Map.new()

    {:reply, :ok, %{state | subscribers: new_subscribers}}
  end

  def handle_call(:clear_all_subscriptions, _from, state) do
    {:reply, :ok, %{state | subscribers: %{}}}
  end

  defp forward_event(json_event, state) do
    {object, event} = EventDecoder.from_json(json_event, state.table_map)

    state.subscribers
    |> Map.get({object, event.action}, [])
    |> Enum.each(fn {fun, _ref} -> fun.(event) end)
  rescue
    error ->
      Honeybadger.notify(
        "DB Watcher callback crash",
        %{event: inspect(json_event), error: inspect(error)},
        self() |> Process.info(:current_stacktrace) |> elem(1)
      )
  end

  defp delete_ref({key, val}, ref) do
    to_delete = Enum.find(val, fn v -> elem(v, 1) == ref end)
    {key, MapSet.delete(val, to_delete)}
  end
end
