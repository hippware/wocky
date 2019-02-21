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
  alias Wocky.User.Location.Supervisor, as: LocationSupervisor

  require Logger
  require Prometheus.Registry

  import Supervisor.Spec, warn: false

  def start(_type, _args) do
    if Confex.get_env(:wocky, :start_watcher) do
      Application.ensure_all_started(:wocky_db_watcher)
    end

    case Confex.get_env(:wocky, :db_only_mode) do
      false -> start_full()
      true -> start_db_only()
    end
  end

  defp start_full() do
    Logger.info("Starting wocky in full mode")

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
           name: Redix},
          %{
            id: LocationSupervisor,
            start: {LocationSupervisor, :start_link, []},
            type: :supervisor
          }
        ],
        strategy: :one_for_one,
        name: Wocky.Supervisor
      )

    Callbacks.register()

    sup
  end

  defp start_db_only() do
    Logger.info("Starting wocky in DB only mode")

    Supervisor.start_link(
      [worker(Wocky.Repo, [])],
      strategy: :one_for_one,
      name: Wocky.Supervisor
    )
  end
end
