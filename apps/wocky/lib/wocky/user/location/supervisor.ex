defmodule Wocky.User.Location.Supervisor do
  @moduledoc """
  This is the supervisor for the location update worker processes
  """
  use Supervisor

  def start_link do
    Supervisor.start_link(__MODULE__, [], name: __MODULE__)
  end

  def init(_) do
    children = [
      worker(Wocky.User.Location.Handler, [], restart: :temporary)
    ]

    supervise(children, strategy: :simple_one_for_one)
  end
end
