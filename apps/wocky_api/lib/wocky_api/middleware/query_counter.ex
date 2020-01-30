defmodule WockyAPI.Middleware.QueryCounter do
  @moduledoc false

  use AbsintheMetrics, adapter: WockyAPI.Metrics.AbsintheQueryCounter
end
