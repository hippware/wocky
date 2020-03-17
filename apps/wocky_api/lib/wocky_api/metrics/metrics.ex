defmodule WockyAPI.Metrics do
  @moduledoc """
  Module for gathering metrics from various parts of the API
  """

  use Elixometer
  use GenServer

  defmodule State do
    @moduledoc false

    defstruct [
      :ws_pids,
      :auth_pids
    ]
  end

  @spec start_link(any()) :: GenServer.on_start()
  def start_link(args) do
    GenServer.start_link(__MODULE__, args, name: __MODULE__)
  end

  @spec add_ws_connection(pid()) :: :ok
  def add_ws_connection(pid) do
    GenServer.cast(__MODULE__, {:add_ws_connection, pid})
  end

  @spec add_auth_connection(pid()) :: :ok
  def add_auth_connection(pid) do
    GenServer.cast(__MODULE__, {:add_auth_connection, pid})
  end

  @impl true
  def init(_) do
    {:ok, %State{ws_pids: MapSet.new(), auth_pids: MapSet.new()}}
  end

  @impl true
  def handle_cast({:add_ws_connection, pid}, state) do
    update_gauge("phoenix.websocket.connections", 1)
    Process.monitor(pid)
    {:noreply, %{state | ws_pids: MapSet.put(state.ws_pids, pid)}}
  end

  def handle_cast({:add_auth_connection, pid}, state) do
    update_gauge("phoenix.websocket.authenticated_connections", 1)
    {:noreply, %{state | auth_pids: MapSet.put(state.auth_pids, pid)}}
  end

  @impl true
  def handle_info({:DOWN, _, :process, pid, _}, state) do
    ws_pids =
      if MapSet.member?(state.ws_pids, pid) do
        update_gauge("phoenix.websocket.connections", -1)
        MapSet.delete(state.ws_pids, pid)
      else
        state.ws_pids
      end

    auth_pids =
      if MapSet.member?(state.auth_pids, pid) do
        update_gauge("phoenix.websocket.authenticated_connections", -1)
        MapSet.delete(state.auth_pids, pid)
      else
        state.auth_pids
      end

    {:noreply, %{state | ws_pids: ws_pids, auth_pids: auth_pids}}
  end
end
