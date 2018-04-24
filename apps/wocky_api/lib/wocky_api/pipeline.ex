defmodule WockyAPI.Pipeline do
  @moduledoc "Custom logging pipeline for GraphQL"

  alias Absinthe.Plug
  alias Absinthe.Pipeline
  alias WockyAPI.PipelineLog

  def pipeline(config, opts) do
    config
    |> Plug.default_pipeline(opts)
    |> Pipeline.insert_before(
      Absinthe.Phase.Parse,
      [{PipelineLog, [{:phase, :request} | opts]}])
    |> Pipeline.insert_after(
      Absinthe.Phase.Document.Result,
      [{PipelineLog, [{:phase, :response} | opts]}])
  end
end
