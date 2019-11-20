defmodule Wocky.Location.Supervisor do
  @moduledoc """
  This is the supervisor for the location update worker processes
  """
  use DynamicSupervisor

  alias Wocky.Account.User

  @spec start_link(any()) :: Supervisor.on_start()
  def start_link(args) do
    DynamicSupervisor.start_link(__MODULE__, args, name: __MODULE__)
  end

  @spec start_child(User.t()) :: {:ok, pid()}
  def start_child(user) do
    spec = {Wocky.Location.Handler, user}
    {:ok, _pid} = DynamicSupervisor.start_child(__MODULE__, spec)
  end

  @impl true
  def init(_) do
    DynamicSupervisor.init(strategy: :one_for_one)
  end
end
