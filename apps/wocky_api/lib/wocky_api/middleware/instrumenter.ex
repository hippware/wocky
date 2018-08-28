defmodule WockyAPI.Middleware.Instrumenter do
  @moduledoc false

  use AbsintheMetrics,
    adapter: AbsintheMetrics.Backend.PrometheusHistogram,
    arguments: [buckets: {:exponential, 1, 2, 12}]
end
