defmodule WockyAPI.PipelineInstrumenter do
  @moduledoc false
  use Prometheus.PlugPipelineInstrumenter

  def label_value(:request_path, conn) do
    conn.request_path
  end
end
