defmodule Wocky.Presence.Supervisor do
  @moduledoc """
  This is the supervisor for the presence manager processes
  """
  use DynamicSupervisor

  @spec start_link(any()) :: Supervisor.on_start()
  def start_link(args) do
    DynamicSupervisor.start_link(__MODULE__, args, name: __MODULE__)
  end

  # This is an overridable function from DynamicSupervisor that isn't part of
  # the behavior. Any added typespec will be ignored.
  # credo:disable-for-next-line Credo.Check.Readability.Specs
  def start_child(user) do
    spec = {Wocky.Presence.Manager, user}
    {:ok, _pid} = DynamicSupervisor.start_child(__MODULE__, spec)
  end

  @impl true
  def init(_) do
    DynamicSupervisor.init(strategy: :one_for_one)
  end
end
