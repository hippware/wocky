defmodule Wocky.DawdleMetrics do
  @moduledoc """
  Handler module for plugging dawdle telemetry into elixometer metrics
  """

  use Elixometer

  @events [
    {:encode, nil, []},
    {:decode, nil, []},
    {:recieve, nil, [{:count, :counter}]},
    {:signal, nil, [{:count, :counter}]},
    {:handle, :handler, []}
  ]

  @spec init :: :ok
  def init do
    Enum.each(@events, &attach/1)
  end

  defp attach({event, sub_field, extra_vals}) do
    full_event = [:dawdle, event]

    string =
      full_event
      |> Enum.map(&to_string/1)
      |> Enum.join(".")

    :ok =
      :telemetry.attach(
        string,
        full_event,
        &handle_event/4,
        %{string: string, sub_field: sub_field, extra_vals: extra_vals}
      )
  end

  defp handle_event([:dawdle | _], measurements, metadata, config) do
    name = full_name(config.string, config.sub_field, metadata)

    Enum.each(
      [{:duration, :histogram} | config.extra_vals],
      fn {metric, type} ->
        value = Map.get(measurements, metric)

        case type do
          :histogram -> update_histogram(name, value)
          :counter -> update_counter(name, value)
        end
      end
    )
  end

  defp full_name(base, nil, _), do: base

  defp full_name(base, sub_field, metadata) do
    Enum.join([base, to_string(Map.get(metadata, sub_field))], ".")
  end
end
