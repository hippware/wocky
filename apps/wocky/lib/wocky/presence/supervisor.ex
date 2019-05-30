defmodule Wocky.Presence.Supervisor do
  @moduledoc """
  This is the supervisor for the presence manager processes
  """
  use Supervisor

  def start_link(args) do
    Supervisor.start_link(__MODULE__, args, name: __MODULE__)
  end

  def init(_) do
    children = [
      worker(Wocky.Presence.Manager, [], restart: :temporary)
    ]

    supervise(children, strategy: :simple_one_for_one)
  end

  def start_child(user) do
    {:ok, _pid} = Supervisor.start_child(__MODULE__, [user])
  end
end
