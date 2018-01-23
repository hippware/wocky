defmodule WockyAPI.Application do
  @moduledoc false

  use Application

  alias WockyAPI.Endpoint
  alias WockyAPI.PhoenixInstrumenter
  alias WockyAPI.PipelineInstrumenter
  alias WockyAPI.PrometheusExporter

  def start(_type, _args) do
    import Supervisor.Spec

    PhoenixInstrumenter.setup()
    PipelineInstrumenter.setup()
    PrometheusExporter.setup()

    # Define workers and child supervisors to be supervised
    children = [
      # Start the endpoint when the application starts
      supervisor(WockyAPI.Endpoint, [])
      # Start your own worker by calling:
      #  WockyAPI.Worker.start_link(arg1, arg2, arg3)
      # worker(WockyAPI.Worker, [arg1, arg2, arg3]),
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: WockyAPI.Supervisor]
    Supervisor.start_link(children, opts)
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  def config_change(changed, _new, removed) do
    Endpoint.config_change(changed, removed)
    :ok
  end
end
