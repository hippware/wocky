defmodule Wocky.Application do
  @moduledoc """
  The Wocky Application Service.

  The wocky system business domain lives in this application.

  Exposes API to clients such as the `Wocky.Web` application
  for use in channels, controllers, and elsewhere.
  """
  use Application

  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    Supervisor.start_link([
      worker(Wocky.Repo, []),
      worker(Wocky.Index, []),
      worker(Wocky.EventHandler, []),
      worker(Wocky.EventHandler.HomeStream, []),
      worker(Wocky.EventHandler.PushNotification, [])
    ], strategy: :one_for_one, name: Wocky.Supervisor)
  end
end
