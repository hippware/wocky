defmodule Wocky.Index.TestIndexer do
  @moduledoc """
  Implements a testing `Indexer`. All index changes are recorded and the
  call is logged, but no further action occurs. The index can then be
  queried by the testing system.
  """

  require Logger

  @behaviour Wocky.Index

  def init do
    Logger.info("Test indexer enabled")
    if :ets.info(:test_index_table) == :undefined do
      _ = :ets.new(:test_index_table, [:public, :named_table, :duplicate_bag])
    end
    reset()
  end

  def update_object(index, id, map) do
    :ets.insert(:test_index_table, {id, index, :update, map})
    :ok
  end

  def delete_object(index, id) do
    :ets.insert(:test_index_table, {id, index, :delete, nil})
    :ok
  end

  def geosearch(_index, _lat, _lon) do
    {:ok, []}
  end

  def get_index_operations do
    :ets.tab2list(:test_index_table)
  end

  def reset do
    :ets.delete_all_objects(:test_index_table)
    :ok
  end
end
