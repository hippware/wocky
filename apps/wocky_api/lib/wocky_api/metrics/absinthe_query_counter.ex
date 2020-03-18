defmodule WockyAPI.Metrics.AbsintheQueryCounter do
  @moduledoc """
  AbsintheMetrics behaviour for counting the total calls to each GraphQL query
  """

  @behaviour AbsintheMetrics

  use Elixometer

  require Logger

  @impl true
  def field(_object, _field, _args \\ []), do: :ok

  @impl true
  def instrument(:query, field, _result, _time) do
    inc_counter(:query, field)
  end

  def instrument(:mutation, field, _result, _time) do
    inc_counter(:mutation, field)
  end

  def instrument(:subscription, field, _result, _time) do
    inc_counter(:subscription, field)
  end

  def instrument(_, _, _, _), do: :ok

  defp inc_counter(object, field) do
    ["graphql.requests", object, field]
    |> Enum.join(".")
    |> update_counter(1)
  end
end
