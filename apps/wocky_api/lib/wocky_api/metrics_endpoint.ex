defmodule WockyAPI.MetricsEndpoint do
  @moduledoc "Endpoint for Prometheus metrics"

  use Phoenix.Endpoint, otp_app: :wocky_api

  # Code reloading can be explicitly enabled under the
  # :code_reloader configuration of your endpoint.
  if code_reloading? do
    plug Phoenix.CodeReloader
  end

  plug Plug.RequestId
  plug Plug.Logger

  plug Plug.MethodOverride
  plug Plug.Head

  # makes the /metrics URL happen
  plug WockyAPI.PrometheusExporter
end
