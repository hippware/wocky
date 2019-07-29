defmodule Wocky.CallbackManager do
  @moduledoc "Manage lists of callbacks"

  use Agent

  @doc false
  def start_link(_args) do
    Agent.start_link(fn -> %{} end, name: __MODULE__)
  end

  @doc "Add a callback to the list associated with the given name"
  @spec add(atom(), fun()) :: :ok
  def add(name, callback) do
    Agent.update(
      __MODULE__,
      &Map.update(&1, name, [callback], fn l -> [callback | l] end)
    )
  end

  @doc "Set the list of callbacks for the given name"
  @spec set(atom(), [fun()]) :: :ok
  def set(name, callbacks) do
    Agent.update(__MODULE__, &Map.put(&1, name, callbacks))
  end

  @doc "Retrieve the list of callbacks for the given name"
  @spec get(atom()) :: [fun()]
  def get(name) do
    Agent.get(__MODULE__, &Map.get(&1, name, []))
  end

  @doc "Reset the callback list for the given name"
  @spec reset(atom()) :: :ok
  def reset(name) do
    Agent.update(__MODULE__, &Map.put(&1, name, []))
  end
end
