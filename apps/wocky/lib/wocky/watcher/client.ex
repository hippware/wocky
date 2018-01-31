defmodule Wocky.Watcher.Client do
  @moduledoc """
  The client for the wocky DB watcher - entities interested in DB callback
  events should subscribe to this process.
  """

  use GenServer

  alias Wocky.Watcher.EventDecoder
  alias Wocky.Watcher.Poller
  alias WockyDBWatcher.Event

  def start_link, do: GenServer.start_link(__MODULE__, nil, name: __MODULE__)

  def send(events), do: GenServer.call(__MODULE__, {:send, events})

  def subscribe(object, action, fun) do
    GenServer.call(__MODULE__, {:subscribe, object, action, fun})
  end

  def unsubscribe(ref) do
    GenServer.call(__MODULE__, {:unsubscribe, ref})
  end

  def init(_) do
    source =
      :wocky_db_watcher
      |> Confex.get_env(:backend, WockyDBWatcher.Backend.SQS)

    source.init
    Poller.start_link(source, __MODULE__)

    {:ok, %{}}
  end

  def handle_call({:send, events}, _from, state) do
    forward_events(events, state)
    {:reply, :ok, state}
  end

  def handle_call({:subscribe, object, action, fun}, _from, state) do
    current = Map.get(state, {object, action}, MapSet.new())
    ref = make_ref()

    {:reply, {:ok, ref},
     Map.put(state, {object, action}, MapSet.put(current, {fun, ref}))}
  end

  def handle_call({:unsubscribe, ref}, _from, state) do
    {:reply, :ok, state |> Enum.map(&delete_ref(&1, ref)) |> Map.new()}
  end

  defp forward_events(events, state) do
    events
    |> Enum.map(&EventDecoder.from_json/1)
    |> Enum.each(&forward_event(&1, state))
  end

  defp forward_event(%Event{object: object, action: action} = event, state) do
    state
    |> Map.get({object, action}, [])
    |> Enum.each(fn {fun, _ref} -> fun.(event) end)
  end

  defp delete_ref({key, val}, ref) do
    to_delete = Enum.find(val, fn v -> elem(v, 1) == ref end)
    {key, MapSet.delete(val, to_delete)}
  end
end
