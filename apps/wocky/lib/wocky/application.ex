defmodule Wocky.Application do
  @moduledoc """
  The Wocky Application Service.

  The wocky system business domain lives in this application.

  Exposes API to clients such as the `Wocky.API` application
  for use in channels, controllers, and elsewhere.
  """
  use Application

  alias Wocky.Mailer

  require Prometheus.Registry

  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    Mailer.init

    Supervisor.start_link([
      worker(Wocky.Repo, []),
      worker(Wocky.Index, []),
      worker(Wocky.Push.Sandbox, []),
      worker(Wocky.Auth.FirebaseKeyManager, [])
    ], strategy: :one_for_one, name: Wocky.Supervisor)
  end
end
