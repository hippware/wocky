defmodule WockyAPI.UtilResolver do
  @moduledoc "Resolver utilities for GraphQL"

  import Ecto.Query

  alias Ecto.Changeset
  alias Wocky.Repo

  def get_count(_root, _args, %{source: %{parent_query: parent_query}}) do
    count =
      parent_query
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

  def extract_nodes({:error, _} = r, _, _), do: r

  def extract_nodes({:ok, connection}, field, orig_field) do
    {:ok,
     Map.update!(connection, :edges, fn edges ->
       for edge <- edges do
         edge
         |> Map.put(orig_field, edge.node)
         |> Map.put(:node, Map.get(edge.node, field))
       end
     end)}
  end

  def fix_changeset(%{errors: [%Changeset{} = cs]} = resolution, _config),
    do: %{resolution | value: cs, errors: []}

  def fix_changeset(resolution, _config), do: resolution
end
