defmodule WockyAPI.Resolvers.Utils do
  @moduledoc "Resolver utilities for GraphQL"

  import Ecto.Query

  alias Absinthe.Relay.Connection
  alias Ecto.Changeset
  alias Wocky.Repo

  def get_count(_root, _args, %{source: %{parent_query: parent_query}}) do
    count =
      parent_query
      |> exclude(:preload)
      |> select([x], count(1))
      |> Repo.one!()

    {:ok, count}
  end

  def get_count(_, _, _), do: {:error, "No parent query found for count"}

  def add_query(source, query), do: add_data(source, :parent_query, query)

  def add_data({:error, _} = r, _key, _value), do: r

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

  def replace_node_with_node_field({:error, _} = r, _, _), do: r

  def replace_node_with_node_field({:ok, connection}, node_field, orig_field) do
    {:ok,
     Map.update!(connection, :edges, fn edges ->
       for edge <- edges do
         edge
         |> Map.put(orig_field, edge.node)
         |> Map.put(:node, Map.get(edge.node, node_field))
       end
     end)}
  end

  def fix_changeset(%{errors: [%Changeset{} = cs]} = resolution, _config),
    do: %{resolution | value: cs, errors: []}

  def fix_changeset(resolution, _config), do: resolution

  def connection_from_query(query, parent, args) do
    query
    |> order_by(desc: :updated_at)
    # Failing dialyzer because Absinthe.Relay.Connection.Options.t is
    # arguably too tighly specced
    |> Connection.from_query(&Repo.all/1, args)
    |> add_query(query)
    |> add_edge_parent(parent)
  end
end
