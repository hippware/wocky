defmodule Wocky.Repo do
  @moduledoc """
  The Wocky interface to Riak. Wocky code should call functions in this module
  instead of calling Riak directly.
  """

  alias Riak.Search
  alias Wocky.Repo.Object

  require Record
  import Record, only: [defrecordp: 2, extract: 2]

  defrecordp :search_results,
    extract(:search_results, from_lib: "riakc/include/riakc.hrl")

  @type type   :: binary
  @type bucket :: binary
  @type key    :: binary

  @doc "Find an object in Riak using the type/bucket/key"
  @spec find(type, bucket, key) :: term
  def find(type, bucket, key), do: Riak.find(type, bucket, key)

  @doc "Find an object in Riak using a Solr query"
  @spec search(binary, binary) :: [map]
  def search(index, conditions) do
    {:ok, results} = Search.query(index, conditions)

    results
    |> search_results(:docs)
    |> List.flatten
    |> Enum.map(&unwrap_search_results/1)
  end

  defp unwrap_search_results({_index, values}) do
    values |> Enum.into(%{}, fn {k, v} -> {String.to_atom(k), v} end)
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
