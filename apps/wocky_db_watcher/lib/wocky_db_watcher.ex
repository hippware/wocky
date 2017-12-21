defmodule WockyDBWatcher do
  @moduledoc """
  WockyDBWatcher is an app that provides database activity events.
  """

  alias WockyDBWatcher.Watcher

  def start_watcher(object, action) do
    result = Supervisor.start_child(
      {:global, WockyDBWatcher.Supervisor},
      %{id: Watcher.name(object, action),
        start: {Watcher, :start_link, [object, action]}
      })

    case result do
      {:ok, child} -> {:ok, child}
      {:error, {:already_started, child}} -> {:ok, child}
      error -> error
    end
  end

end
