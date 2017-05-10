defmodule Wocky.Index.NullIndexer do
  @moduledoc """
  Implements a null `Index`. All operations are no-ops that ignore
  their inputs. This handler effectively disables indexing.
  """

  require Logger

  @behaviour Wocky.Index

  def init do
    Logger.info("Indexing disabled")
  end

  def update_object(_index, _id, _map) do
    :ok
  end

  def delete_object(_index, _id) do
    :ok
  end

  def geosearch(_index, _lat, _lon) do
    {:error, :indexing_disabled}
  end
end
