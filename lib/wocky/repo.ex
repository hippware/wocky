defmodule Wocky.Repo do
  @moduledoc """
  The Wocky interface to Riak. Wocky code should call functions in this module
  instead of calling Riak directly.
  """

  import Record, only: [defrecordp: 2, extract: 2]

  alias Riak.Bucket
  alias Riak.Search
  alias Wocky.Repo.Doc

  require Record

  @type type   :: binary
  @type bucket :: binary
  @type key    :: binary

  defrecordp :search_results,
    extract(:search_results, from_lib: "riakc/include/riakc.hrl")

  # =========================================================================
  # Document API

  @doc "Fetch an object from Riak using the type/bucket/key"
  @spec get(type, bucket, key) :: Doc.t | nil
  def get(type, bucket, key) do
    Riak.find(type, bucket, key)
  end

  @doc "Update the type/bucket/key in Riak with the provided object"
  @spec put(Doc.t, type, bucket, key) :: :ok | {:error, term}
  def put(data, type, bucket, key) do
    Riak.update(data, type, bucket, key)
  end

  @doc "Delete the object from Riak that is identified by the type/bucket/key"
  @spec delete(type, bucket, key) :: :ok | {:error, term}
  def delete(type, bucket, key) do
    Riak.delete(type, bucket, key)
  end

  @doc """
  Delete all keys from a bucket.

  This is a very expensive operation. DO NOT USE IN PRODUCTION!
  """
  @spec delete_all(type, bucket) :: :ok
  def delete_all(type, bucket) do
    for key <- Bucket.keys!(type, bucket) do
      _ = Riak.delete(type, bucket, key)
    end
    :ok
  end

  @doc "Find an object in Riak using a Solr query"
  @spec search(binary, binary) :: [Doc.t]
  def search(index, conditions) do
    index
    |> do_search(conditions)
    |> Enum.reduce([], &lookup_search_result/2)
  end

  defp do_search(index, conditions) do
    {:ok, results} = Search.query(index, conditions)
    results |> search_results(:docs) |> List.flatten
  end

  defp lookup_search_result({_index, values}, acc) do
    btype  = :proplists.get_value("_yz_rt", values)
    bucket = :proplists.get_value("_yz_rb", values)
    key    = :proplists.get_value("_yz_rk", values)
    case Riak.find(btype, bucket, key) do
      nil -> acc
      obj -> [obj | acc]
    end
  end

  @doc "Wait for a new user to be indexed for searching"
  @spec wait_for_search_result(binary, binary, pos_integer, pos_integer) ::
    :ok | {:error, :timeout}
  def wait_for_search_result(index, query, sleep_time \\ 250, retries \\ 10)
  def wait_for_search_result(_, _, _, 0), do: {:error, :timeout}
  def wait_for_search_result(index, query, sleep_time, retries) do
    case search(index, query) do
      [] ->
        Process.sleep(sleep_time)
        wait_for_search_result(index, query, sleep_time, retries - 1)

      [_data | _] ->
        :ok
    end
  end

  # =========================================================================
  # Map API

  @doc "Find an object in Riak using the type/bucket/key"
  @spec find(type, bucket, key) :: map | nil
  def find(type, bucket, key) do
    case get(type, bucket, key) do
      nil -> nil
      doc -> Doc.to_map(doc)
    end
  end

  @doc "Update the type/bucket/key in Riak with the provided object"
  @spec update(map, type, bucket, key) :: :ok | {:error, term}
  def update(data, type, bucket, key) do
    data |> Doc.new |> put(type, bucket, key)
  end

  @doc "Find and object in Riak using a Solr query"
  @spec unsafe_search(binary, binary) :: [map]
  def unsafe_search(index, conditions) do
    index
    |> do_search(conditions)
    |> Enum.map(&unwrap_search_results/1)
  end

  defp unwrap_search_results({_index, values}) do
    Enum.reduce(values, %{}, &unwrap_result/2)
  end

  defp unwrap_result({"_yz_" <> _, _}, acc), do: acc
  defp unwrap_result({"score", _}, acc), do: acc
  defp unwrap_result({name, value}, acc) do
    key =
      name
      |> String.replace("_register", "")
      |> String.to_atom

    Map.put(acc, key, value)
  end
end
