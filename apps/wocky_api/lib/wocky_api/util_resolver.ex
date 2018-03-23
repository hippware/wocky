defmodule WockyAPI.UtilResolver do
  @moduledoc "Resolver utilities for GraphQL"

  import Ecto.Query

  alias Wocky.Repo

  def get_count(_root, _args, %{source: %{parent_query: parent_query}}) do
    count =
      parent_query
      |> select([x], count(x.id))
      |> Repo.one!

    {:ok, count}
  end
  def get_count(_, _, _), do: {:error, "No parent query found for count"}

  def add_query(source, query), do: add_data(source, :parent_query, query)

  def add_data({:error, _} = r, _key, _value), do: r
  def add_data({:ok, source}, key, value),
    do: {:ok, Map.put(source, key, value)}
end
