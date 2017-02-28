defmodule Wocky.Repo do
  @moduledoc """
  The Wocky interface to Riak. Wocky code should call functions in this module
  instead of calling Riak directly.
  """

  alias Riak.Search
  alias Wocky.Repo.Object

  @type type   :: binary
  @type bucket :: binary
  @type key    :: binary

  @doc "Find an object in Riak using the type/bucket/key"
  @spec find(type, bucket, key) :: term
  def find(type, bucket, key), do: Riak.find(type, bucket, key)

  @doc "Find an object in Riak using a Solr query"
  @spec search(binary, binary) :: nil | map
  def search(index, conditions) do
    {:ok, {:search_results, results, _, _}} =
      Search.query(index, conditions)

    case results do
      [] -> nil
      [{^index, values}] ->
        values |> Enum.into(%{}, fn {k, v} -> {String.to_atom(k), v} end)
    end
  end

  @doc "Update the type/bucket/key in Riak with the provided object"
  @spec update(map | Object.t, type, bucket, key) :: :ok | {:error, term}
  def update(map, type, bucket, key) when is_map(map),
    do: map |> Object.new |> update(type, bucket, key)

  def update(object, type, bucket, key),
    do: Riak.update(object, type, bucket, key)

  @doc "Delete the object from Riak that is identified by the type/bucket/key"
  @spec delete(type, bucket, key) :: :ok | {:error, term}
  def delete(type, bucket, key), do: Riak.delete(type, bucket, key)
end
