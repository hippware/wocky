defmodule WockyDBWatcher.Application do
  @moduledoc """
  The Wocky DB Watcher Service.
  """
  use Application

  def start(_type, _args) do
    Supervisor.start_link(
      [],
      strategy: :one_for_one,
      name: {:global, WockyDBWatcher.Supervisor}
    )
  end
end
