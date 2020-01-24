defmodule WockyAPI.Resolvers.Utils do
  @moduledoc "Resolver utilities for GraphQL"

  import Ecto.Query

  alias Absinthe.Relay.Connection
  alias Absinthe.Subscription
  alias AbsintheErrorPayload.Payload
  alias Ecto.Changeset
  alias Ecto.Queryable
  alias Wocky.Repo
  alias WockyAPI.Endpoint

  @spec get_count(map(), any(), any()) ::
          {:ok, non_neg_integer()} | {:error, binary()}
  def get_count(%{cached_count: count}, _args, _info), do: {:ok, count}

  def get_count(%{parent_query: parent_query}, _args, _info),
    do: {:ok, get_count(parent_query)}

  def get_count(_, _, _), do: {:error, "No parent query found for count"}

  @spec get_count(Queryable.t()) :: non_neg_integer()
  def get_count(query) do
    query
    |> exclude(:preload)
    |> exclude(:order_by)
    |> select([x], count(1))
    |> Repo.one()
    |> Kernel.||(0)
  end

  @spec fix_changeset(map(), any()) :: map()
  def fix_changeset(%{errors: [%Changeset{} = cs]} = resolution, _config),
    do: %{resolution | value: cs, errors: []}

  def fix_changeset(resolution, _config), do: resolution

  @spec fix_changeset_list(map(), any()) :: map()
  def fix_changeset_list(%{errors: []} = resolutions, config) do
    new_value =
      resolutions.value
      |> Enum.map(&fix_changeset(&1, config))
      |> Enum.map(&Payload.convert_to_payload/1)

    %{resolutions | value: new_value}
  end

  def fix_changeset_list(resolutions, _config), do: resolutions

  @spec publish_subscription(String.t() | [String.t()], atom(), any()) :: :ok
  def publish_subscription(topic, subscription, data) when is_binary(topic),
    do: publish_subscription([topic], subscription, data)

  def publish_subscription(topics, subscription, data) do
    targets = Enum.map(topics, &{subscription, &1})

    Subscription.publish(Endpoint, data, targets)
  end

  @spec connection_from_query(Queryable.t(), any(), map(), Keyword.t()) ::
          {:ok, map()}
  def connection_from_query(
        query,
        parent,
        args,
        opts \\ []
      ) do
    args = Map.take(args, [:first, :last, :after, :before])
    order_by = Keyword.get(opts, :order_by, desc: :updated_at)
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

  defp maybe_order_by(query, nil), do: query

  defp maybe_order_by(query, order_by), do: order_by(query, ^order_by)

  defp cursor_fields(order_by),
    do: Enum.map(order_by, fn {direction, field} -> {field, direction} end)

  defp get_count_if_needed(query, args) do
    if args[:last] != nil && args[:before] == nil && args[:after] == nil do
      get_count(query)
    else
      nil
    end
  end

  defp maybe_postprocess({:ok, %{edges: edges} = connection}, fun)
       when not is_nil(fun) do
    {:ok,
     %{
       connection
       | edges: Enum.map(edges, fn e -> %{e | node: fun.(e.node)} end)
     }}
  end

  defp maybe_postprocess(connection, nil), do: connection

  defp add_data({:ok, source}, _key, nil), do: {:ok, source}

  defp add_data({:ok, source}, key, value),
    do: {:ok, Map.put(source, key, value)}

  defp add_data(result, _key, _value), do: result

  defp add_edge_parent({:ok, connection}, parent) do
    {:ok,
     Map.update!(connection, :edges, fn edges ->
       for edge <- edges do
         Map.put(edge, :parent, parent)
       end
     end)}
  end

  defp add_edge_parent(result, _), do: result
end
