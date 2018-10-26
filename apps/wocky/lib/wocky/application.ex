defmodule Wocky.Application do
  @moduledoc """
  The Wocky Application Service.

  The wocky system business domain lives in this application.

  Exposes API to clients such as the `Wocky.API` application
  for use in channels, controllers, and elsewhere.
  """
  use Application

  alias Wocky.Callbacks
  alias Wocky.Mailer

  require Prometheus.Registry

  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    if Confex.get_env(:wocky, :start_watcher) do
      Application.ensure_all_started(:wocky_db_watcher)
    end

    redis_config = Confex.get_env(:wocky, :redis)

    Mailer.init()

    sup =
      Supervisor.start_link(
        [
          worker(Wocky.Repo, []),
          worker(Wocky.Push.Sandbox, []),
          worker(Wocky.Watcher.Client, []),
          %{id: Dawdle, start: {Dawdle, :start_link, []}},
          {Redix,
           host: redis_config[:host],
           port: redis_config[:port],
           database: redis_config[:db],
           name: Redix}
        ],
        strategy: :one_for_one,
        name: Wocky.Supervisor
      )

    Callbacks.register()

    sup
  end
end
