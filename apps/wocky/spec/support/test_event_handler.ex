defmodule TestEventHandler do
  @moduledoc false

  def init do
    if :ets.info(:test_events_table) == :undefined do
      _ = :ets.new(:test_events_table, [:public, :named_table, :set])
    end

    reset()
    :ok
  end

  def broadcast(event) do
    :ets.insert(:test_events_table, {DateTime.utc_now(), event})
    :ok
  end

  @spec get_events :: [any]
  def get_events do
    :ets.tab2list(:test_events_table)
  end

  @spec reset :: :ok
  def reset do
    :ets.delete_all_objects(:test_events_table)
    :ok
  end
end
