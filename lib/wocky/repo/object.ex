defmodule Wocky.Repo.Object do
  @moduledoc "Simple interface to Riak CRDTs"

  require Record
  alias Riak.CRDT.Map
  alias Riak.CRDT.Register

  @type t :: :riakc_map.crdt_map

  @doc "Convert an Elixir map to a Riak CRDT"
  @spec new(map) :: t
  def new(data) when is_map(data) do
    make(data)
  end

  defp make(data) when is_map(data) do
    data |> Enum.reduce(Map.new,
                        fn {k, v}, acc ->
                          Map.put(acc, to_string(k), make(v))
                        end)
  end
  defp make(data), do: Register.new(to_string(data))

  @doc "Transforms an 'unwrapped' CRDT map into an Elixir map"
  @spec to_map(t | list | nil) :: map
  def to_map(nil), do: %{}
  def to_map(data) when Record.is_record(data, :map) do
    data |> Map.value |> to_map
  end
  def to_map(data) when is_list(data) do
    data |> Enum.into(%{},
                      fn {{k, t}, v} ->
                        {String.to_atom(k), map_value(t, v)}
                      end)
  end

  defp map_value(:map, value), do: to_map(value)
  defp map_value(:register, value), do: value
end
