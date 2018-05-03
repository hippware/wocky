defmodule WockyAPI.Middleware.Instrumenter do
  @moduledoc false

  use AbsintheMetrics,
    adapter: AbsintheMetrics.Backend.PrometheusHistogram,
    arguments: [buckets: {:exponential, 250, 1.5, 7}]
end
