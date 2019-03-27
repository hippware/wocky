defmodule Wocky.Tasks.Recurring do
  @moduledoc """
  A really trivial little module to handle tasks that recur at an interval.
  If we end up adding more tasks we can generalise this, but I didn't want to
  impelement an entire complex cronjob-style system for one single process.
  """

  use GenServer

  alias Wocky.Tasks.LocShareExpire

  @timeout :timer.minutes(1)

  def start,
    do: Swarm.whereis_or_register_name(__MODULE__, __MODULE__, :start_link, [])

  def start_link, do: GenServer.start_link(__MODULE__, nil)

  def init(_), do: {:ok, :no_state, 0}

  def handle_info(:timeout, state) do
    LocShareExpire.run()

    {:noreply, state, @timeout}
  end

  def handle_info({:sawrm, :resolve_conflict, _state}, state),
    do: {:noreply, state, @timeout}

  def handle_info({:sawrm, :die}, state), do: {:stop, :shutdown, state}

  def handle_call({:swarm, :begin_handoff}, _from, state),
    do: {:reply, :restart, state, @timeout}
end
