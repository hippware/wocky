defmodule Wocky.SwarmSupContainer do
  @moduledoc """
  A container process for running a supervisor under Swarm. The MFA passed in
  must return `{:ok, pid}` where `pid` is Supervisor style process (in
  particular it must be correctly handled as a supervisor process by
  `Supervisor.stop/3`).
  """

  use GenServer

  @spec start_link(module(), atom(), list()) :: {:ok, pid()}
  def start_link(module, fun, args) do
    GenServer.start_link(__MODULE__, [module, fun, args])
  end

  def init([module, fun, args]) do
    {:ok, _pid} = :erlang.apply(module, fun, args)
  end

  # called when a handoff has been initiated due to changes
  # in cluster topology, valid response values are:
  #
  #   - `:restart`, to simply restart the process on the new node
  #   - `{:resume, state}`, to hand off some state to the new process
  #   - `:ignore`, to leave the process running on its current node
  #
  def handle_call({:swarm, :begin_handoff}, _from, pid) do
    {:reply, :restart, pid}
  end

  # called when a network split is healed and the local process
  # should continue running, but a duplicate process on the other
  # side of the split is handing off its state to us. You can choose
  # to ignore the handoff state, or apply your own conflict resolution
  # strategy
  def handle_cast({:swarm, :resolve_conflict, _state}, pid) do
    {:noreply, pid}
  end

  # this message is sent when this process should die
  # because it is being moved, use this as an opportunity
  # to clean up
  def handle_info({:swarm, :die}, pid) do
    Supervisor.stop(pid, :shutdown, 5000)
    {:stop, :shutdown, pid}
  end
end
