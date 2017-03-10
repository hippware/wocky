defmodule Wocky.Repo.Doc do
  @moduledoc "Simple interface to Riak CRDTs"

  require Record
  alias Riak.CRDT.Map, as: RMap
  alias Riak.CRDT.Register

  @type t        :: :riakc_map.crdt_map
  @type crdt_map :: :riakc_map.crdt_map
  @type key      :: atom | binary
  @type value    :: map | binary

  @doc "Initialize a new document"
  @spec new :: t
  def new, do: RMap.new

  @doc "Initialize a document from an Elixir Map or CRDT Map"
  @spec new(map | crdt_map) :: t
  def new(data) when Record.is_record(data, :map), do: data
  def new(data) when is_map(data), do: data |> from_map()

  @doc "Transforms an Elixir map into a document"
  @spec from_map(map | nil) :: t
  def from_map(nil), do: new()
  def from_map(%{__struct__: _} = map), do: map |> Map.from_struct |> from_map()
  def from_map(map) when is_map(map), do: map |> make()

  defp make(data) when is_map(data) do
    data |> Enum.reduce(RMap.new,
                        fn {k, v}, acc ->
                          RMap.put(acc, to_string(k), make(v))
                        end)
  end
  defp make(data), do: Register.new(to_string(data))

  @doc "Transforms a document into an Elixir map"
  @spec to_map(t | nil) :: map
  def to_map(nil), do: %{}
  def to_map(doc) when Record.is_record(doc, :map) do
    doc |> RMap.value |> unmake()
  end

  defp unmake([]), do: %{}
  defp unmake([{{_, _}, _} | _] = data) do
    data |> Enum.into(%{},
                      fn {{k, t}, v} ->
                        {String.to_atom(k), map_value(t, v)}
                      end)
  end

  defp map_value(:map, value), do: unmake(value)
  defp map_value(:register, value), do: value

  @doc "Get the `doc` size"
  @spec size(t | nil) :: pos_integer
  def size(doc), do: doc |> RMap.size

  @doc "Fetch the value associated to `key` in `doc`"
  @spec get(t, key) :: value
  def get(doc, key), do: doc |> RMap.get(:register, to_string(key))

  @doc "Update the `key` on the `doc` by passing the `value`"
  @spec put(t, key, value) :: t | {:error, term}
  def put(doc, key, value),
    do: doc |> RMap.put(to_string(key), wrap_value(value))

  defp wrap_value(value) when is_map(value), do: from_map(value)
  defp wrap_value(value), do: Register.new(to_string(value))

  @doc "Delete a `key` from the `doc`"
  @spec delete(t, key) :: t | {:error, term}
  def delete(doc, key), do: doc |> RMap.delete({to_string(key), :register})

  @doc "Delete an embedded doc at `key` from the `doc`"
  @spec delete_doc(t, key) :: t | {:error, term}
  def delete_doc(doc, key), do: doc |> RMap.delete({to_string(key), :map})

  @doc "Get the original value of the `doc`"
  @spec value(t) :: list
  def value(doc), do: doc |> RMap.value

  @doc "List all keys of the `doc`"
  @spec keys(t) :: list
  def keys(doc), do: doc |> RMap.keys

  @doc "Test if the `key` is contained in the `doc`"
  @spec key?(t, key) :: boolean
  def key?(doc, key), do: doc |> RMap.has_key?(to_string(key))
end
