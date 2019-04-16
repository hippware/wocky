defmodule WockyAPI.Resolvers.Utils do
  @moduledoc "Resolver utilities for GraphQL"

  import Ecto.Query

  alias Absinthe.Relay.Connection
  alias Ecto.Changeset
  alias Geo.Point
  alias Kronky.Payload
  alias Wocky.{GeoUtils, Repo}

  def get_count(%{cached_count: count}, _args, _info) do
    {:ok, count}
  end

  def get_count(%{parent_query: parent_query}, _args, _info) do
    {:ok, get_count(parent_query)}
  end

  def get_count(_, _, _), do: {:error, "No parent query found for count"}

  def add_data({:error, _} = r, _key, _value), do: r

  def add_data({:ok, source}, _keykey, nil), do: {:ok, source}

  def add_data({:ok, source}, key, value),
    do: {:ok, Map.put(source, key, value)}

  def add_edge_parent({:error, _} = r, _), do: r

  def add_edge_parent({:ok, connection}, parent) do
    {:ok,
     Map.update!(connection, :edges, fn edges ->
       for edge <- edges do
         Map.put(edge, :parent, parent)
       end
     end)}
  end

  def fix_changeset(%{errors: [%Changeset{} = cs]} = resolution, _config),
    do: %{resolution | value: cs, errors: []}

  def fix_changeset(resolution, _config), do: resolution

  def fix_changeset_list(%{errors: []} = resolutions, config) do
    new_value =
      resolutions.value
      |> Enum.map(&fix_changeset(&1, config))
      |> Enum.map(&Payload.convert_to_payload/1)

    %{resolutions | value: new_value}
  end

  def fix_changeset_list(resolutions, _config), do: resolutions

  def connection_from_query(
        query,
        parent,
        args,
        opts \\ []
      ) do
    args = Map.take(args, [:first, :last, :after, :before])
    order_by = Keyword.get(opts, :order_by, [desc: :updated_at])
    postprocess = Keyword.get(opts, :postprocess)

    query = maybe_order_by(query, order_by)
    cursor_fields = cursor_fields(order_by)

    opts = [
      count: get_count_if_needed(query, args),
      cursor_fields: cursor_fields
    ]

    query
    |> Connection.from_query(Repo, args, opts)
    |> maybe_postprocess(postprocess)
    |> add_data(:parent_query, query)
    |> add_data(:cached_count, opts[:count])
    |> add_edge_parent(parent)
  end

  @spec map_point(map()) :: Point.t()
  def map_point(point_arg) do
    {lat, lon} = GeoUtils.normalize_lat_lon(point_arg[:lat], point_arg[:lon])
    GeoUtils.point(lat, lon)
  end

  defp get_count_if_needed(query, args) do
    if args[:last] != nil && args[:before] == nil && args[:after] == nil do
      get_count(query)
    else
      nil
    end
  end

  defp get_count(query) do
    query
    |> exclude(:preload)
    |> exclude(:order_by)
    |> select([x], count(1))
    |> Repo.one()
    |> Kernel.||(0)
  end

  defp maybe_order_by(query, nil), do: query

  defp maybe_order_by(query, order_by) do
    query
    |> order_by(^order_by)
  end

  defp cursor_fields(order_by),
    do: Enum.map(order_by, fn {direction, field} -> {field, direction} end)

  defp maybe_postprocess({:ok, %{edges: edges} = connection}, fun)
       when not is_nil(fun) do
    {:ok,
     %{
       connection
       | edges: Enum.map(edges, fn e -> %{e | node: fun.(e.node)} end)
     }}
  end

  defp maybe_postprocess(connection, nil), do: connection

  def map_edges({:error, _} = r, _), do: r

  def map_edges({:ok, %{edges: edges} = result}, fun),
    do: {:ok, %{result | edges: Enum.map(edges, &update_in(&1[:node], fun))}}
end
