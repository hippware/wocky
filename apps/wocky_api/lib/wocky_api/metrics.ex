defmodule WockyAPI.Metrics do
  @moduledoc """
  Module for gathering metrics from various parts of the API
  """

  defmodule State do
    @moduledoc false

    defstruct [
      :ws_pids,
      :auth_pids
    ]
  end

  use Elixometer
  use GenServer

  def start_link(args) do
    GenServer.start_link(__MODULE__, args, name: __MODULE__)
  end

  def add_ws_connection(pid) do
    GenServer.cast(__MODULE__, {:add_ws_connection, pid})
  end

  def add_auth_connection(pid) do
    GenServer.cast(__MODULE__, {:add_auth_connection, pid})
  end

  def init(_) do
    {:ok, %State{ws_pids: MapSet.new(), auth_pids: MapSet.new()}}
  end

  def handle_cast({:add_ws_connection, pid}, state) do
    update_counter("phoenix.websocket.connections", 1)
    Process.monitor(pid)
    {:noreply, %{state | ws_pids: MapSet.put(state.ws_pids, pid)}}
  end

  def handle_cast({:add_auth_connection, pid}, state) do
    update_counter("phoenix.websocket.authenticated_connections", 1)
    {:noreply, %{state | auth_pids: MapSet.put(state.auth_pids, pid)}}
  end

  def handle_info({:DOWN, _, :process, pid, _}, state) do
    ws_pids =
      if MapSet.member?(state.ws_pids, pid) do
        update_counter("phoenix.websocket.connections", -1)
        MapSet.delete(state.ws_pids, pid)
      end

    auth_pids =
      if MapSet.member?(state.auth_pids, pid) do
        update_counter("phoenix.websocket.authenticated_connections", -1)
        MapSet.delete(state.auth_pids, pid)
      end

    {:noreply, %{state | ws_pids: ws_pids, auth_pids: auth_pids}}
  end
end
