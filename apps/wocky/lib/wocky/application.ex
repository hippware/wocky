defmodule Wocky.Application do
  @moduledoc """
  The Wocky Application Service.

  The wocky system business domain lives in this application.

  Exposes API to clients such as the `Wocky.API` application
  for use in channels, controllers, and elsewhere.
  """
  use Application

  alias Wocky.DawdleMetrics
  alias Wocky.Location.Supervisor, as: LocationSupervisor
  alias Wocky.Notifier.Email.Mailer
  alias Wocky.Notifier.Push.Backend.Sandbox, as: PushSandbox
  alias Wocky.Presence.Supervisor, as: PresenceSupervisor

  require Logger
  require Prometheus.Registry

  @impl true
  def start(_type, _args) do
    case Confex.get_env(:wocky, :db_only_mode, false) do
      false -> start_full()
      true -> start_db_only()
    end
  end

  defp start_full do
    Logger.info("Starting wocky in full mode")
    Prometheus.Registry.register_collector(:prometheus_process_collector)

    DawdleMetrics.init()

    Mailer.init()

    redis_config = redis_config()
    topologies = Confex.get_env(:libcluster, :topologies)

    children = [
      {Cluster.Supervisor, [topologies, [name: Wocky.ClusterSupervisor]]},
      {Redix, redis_config},
      {Redlock, Confex.get_env(:wocky, :redlock)},
      %{
        id: Phoenix.PubSub.Redis,
        start:
          {Phoenix.PubSub.Redis, :start_link,
           [
             :presence,
             redis_config
             |> Enum.into([])
             |> Keyword.merge(pool_size: 1, node_name: redis_node())
           ]}
      },
      {LocationSupervisor, []},
      {PresenceSupervisor, []},
      {PushSandbox, []}
    ]

    sup = start_supervisor(children)

    # Set up prometheus_ecto
    :ok =
      :telemetry.attach(
        "prometheus-ecto",
        [:wocky, :repo, :query],
        &Wocky.Repo.Instrumenter.handle_event/4,
        nil
      )

    if Confex.get_env(:wocky, :start_watcher, true) do
      :ok = DawdleDB.start_watcher(Wocky.Repo)
    end

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

  defp redis_node do
    case node() do
      :nonode@nohost -> 'test_node@host'
      node -> node
    end
  end
end
