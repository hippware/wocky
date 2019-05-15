defmodule Wocky.Application do
  @moduledoc """
  The Wocky Application Service.

  The wocky system business domain lives in this application.

  Exposes API to clients such as the `Wocky.API` application
  for use in channels, controllers, and elsewhere.
  """
  use Application

  alias DawdleDB.Watcher.Supervisor, as: Watcher
  alias Wocky.CallbackManager
  alias Wocky.Notifier.Email.Mailer
  alias Wocky.Notifier.Push.Backend.Sandbox, as: PushSandbox
  alias Wocky.Tasks.Recurring
  alias Wocky.User.Location.Supervisor, as: LocationSupervisor
  alias Wocky.User.Presence.Supervisor, as: PresenceSupervisor

  require Logger
  require Prometheus.Registry

  def start(_type, _args) do
    case Confex.get_env(:wocky, :db_only_mode, false) do
      false -> start_full()
      true -> start_db_only()
    end
  end

  defp start_full do
    Logger.info("Starting wocky in full mode")
    Prometheus.Registry.register_collector(:prometheus_process_collector)

    Mailer.init()

    redis_config = redis_config()

    children = [
      {Redix, redis_config},
      {Redlock, Confex.get_env(:wocky, :redlock)},
      %{
        id: Phoenix.PubSub.Redis,
        start:
          {Phoenix.PubSub.Redis, :start_link,
           [
             :presence,
             [
               host: redis_config[:host],
               port: redis_config[:port],
               pool_size: 1,
               node_name: redis_node()
             ]
           ]}
      },
      {LocationSupervisor, []},
      {PresenceSupervisor, []},
      {CallbackManager, []},
      {PushSandbox, []}
    ]

    sup = {:ok, pid} = start_supervisor(children)

    # Set up prometheus_ecto
    :ok =
      :telemetry.attach(
        "prometheus-ecto",
        [:wocky, :repo, :query],
        &Wocky.Repo.Instrumenter.handle_event/4,
        nil
      )

    {:ok, _} = Recurring.start()

    Dawdle.start_pollers()

    Confex.get_env(:wocky, :start_watcher, false)
    |> maybe_start_watcher(pid)

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

  defp maybe_start_watcher(false, _), do: :ok

  defp maybe_start_watcher(true, sup) do
    {:ok, _} = Supervisor.start_child(sup, {Watcher, Wocky.Repo.config()})
    :ok
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
