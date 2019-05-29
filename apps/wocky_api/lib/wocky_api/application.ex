defmodule WockyAPI.Application do
  @moduledoc false

  use Application

  alias Wocky.Location.UserLocation.Current, as: CurrentLocation
  alias Wocky.User.Presence
  alias WockyAPI.Callbacks
  alias WockyAPI.Endpoint
  alias WockyAPI.Middleware.Instrumenter, as: AbsintheInstrumenter
  alias WockyAPI.PhoenixInstrumenter
  alias WockyAPI.PipelineInstrumenter
  alias WockyAPI.PrometheusExporter

  def start(_type, _args) do
    PhoenixInstrumenter.setup()
    PipelineInstrumenter.setup()
    PrometheusExporter.setup()
    _ = AbsintheInstrumenter.install(WockyAPI.Schema)

    # Define workers and child supervisors to be supervised
    children = [
      # Start the endpoints when the application starts
      WockyAPI.Endpoint,
      {Absinthe.Subscription, WockyAPI.Endpoint},
      WockyAPI.MetricsEndpoint
    ]

    opts = [strategy: :one_for_one, name: WockyAPI.Supervisor]

    Callbacks.register()

    Presence.register_callback(&WockyAPI.Resolvers.Presence.publish_presence/2)

    CurrentLocation.register_callback(
      &WockyAPI.Resolvers.User.notify_location/2
    )

    Supervisor.start_link(children, opts)
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  def config_change(changed, _new, removed) do
    Endpoint.config_change(changed, removed)
    :ok
  end
end
