defmodule WockyAPI.Pipeline do
  @moduledoc "Custom logging pipeline for GraphQL"

  alias Absinthe.Phoenix.Channel
  alias Absinthe.Pipeline
  alias Absinthe.Plug
  alias WockyAPI.PipelineLog

  @trace_environments ["testing"]

  def pipeline(config, opts) do
    config
    |> Plug.default_pipeline(opts)
    |> add_logger(opts)
    |> maybe_add_apollo(opts)
  end

  def channel_pipeline(schema_mod, opts) do
    schema_mod
    |> Channel.default_pipeline(opts)
    |> add_logger(opts)
  end

  defp add_logger(pipeline, opts) do
    pipeline
    |> Pipeline.insert_before(Absinthe.Phase.Parse, [
      {PipelineLog, [{:phase, :request} | opts]}
    ])
    |> Pipeline.insert_after(Absinthe.Phase.Document.Result, [
      {PipelineLog, [{:phase, :response} | opts]}
    ])
  end

  defp maybe_add_apollo(pipeline, opts) do
    current_user = opts[:context][:current_user]

    if Confex.get_env(:wocky, :wocky_inst) in @trace_environments
       && !is_nil(current_user) do
      ApolloTracing.Pipeline.add_phases(pipeline)
    else
      pipeline
    end
  end
end
