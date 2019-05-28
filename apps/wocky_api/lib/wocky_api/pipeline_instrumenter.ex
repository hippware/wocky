defmodule WockyAPI.PipelineInstrumenter do
  @moduledoc false
  use Prometheus.PlugPipelineInstrumenter

  # Replace user, bot, etc UUIDs with a common string to limit the number of
  # paths we generate metrics for
  def label_value(:request_path, conn) do
    Regex.replace(~r/[a-f\d]{8}(-[a-f\d]{4}){3}-[a-f\d]{12}/, conn.request_path, "<UUID>")
  end
end
