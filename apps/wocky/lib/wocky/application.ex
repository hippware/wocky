defmodule Wocky.Application do
  @moduledoc """
  The Wocky Application Service.

  The wocky system business domain lives in this application.

  Exposes API to clients such as the `Wocky.API` application
  for use in channels, controllers, and elsewhere.
  """
  use Application

  alias DawdleDB.Watcher.Supervisor, as: Watcher
  alias Wocky.Mailer
  alias Wocky.Push.Sandbox, as: PushSandbox
  alias Wocky.User.Location.Supervisor, as: LocationSupervisor

  require Logger
  require Prometheus.Registry

  def start(_type, _args) do
    sup =
      {:ok, pid} =
      case Confex.get_env(:wocky, :db_only_mode, false) do
        false -> start_full()
        true -> start_db_only()
      end

    if Confex.get_env(:wocky, :start_watcher, false) do
      Supervisor.start_child(pid, {Watcher, Wocky.Repo.config()})
    end

    sup
  end

  defp start_full do
    Logger.info("Starting wocky in full mode")
    Prometheus.Registry.register_collector(:prometheus_process_collector)

    Mailer.init()

    children = [
      {Redix, redis_config()},
      {LocationSupervisor, []},
      {PushSandbox, []}
    ]

    sup = {:ok, _pid} = start_supervisor(children)

    # Set up prometheus_ecto
    :ok =
      :telemetry.attach(
        "prometheus-ecto",
        [:wocky, :repo, :query],
        &Wocky.Repo.Instrumenter.handle_event/4,
        nil
      )

    sup
  end

  defp start_db_only do
    Logger.info("Starting wocky in DB only mode")

    start_supervisor([])
  end

  defp start_supervisor(children) do
    Supervisor.start_link(
      [Wocky.Repo | children],
      strategy: :one_for_one,
      name: Wocky.Supervisor
    )
  end

  defp redis_config do
    :wocky
    |> Confex.fetch_env!(:redis)
    |> Keyword.take([:host, :port, :ssl, :database, :password])
    |> Keyword.put(:sync_connect, true)
    |> Keyword.put(:name, Redix)
  end
end
