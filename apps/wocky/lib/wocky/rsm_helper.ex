defmodule Wocky.RSMHelper.Guard do
  @moduledoc false

  defmacro is_order_forward(dir, sort_order) do
    quote do
      (unquote(dir) == :aft and unquote(sort_order) == :asc) or
      (unquote(dir) == :before and unquote(sort_order) == :desc)
    end
  end
end

defmodule Wocky.RSMHelper do
  @moduledoc """
  This module provides a helper interface to allow RSM queries to be handled
  entirely by the database, removing the need to retrieve any more records
  than necessary to satisfy the request.
  """

  use Wocky.Repo.Model

  import Wocky.RSMHelper.Guard

  require Record

  alias Wocky.Repo.Timestamp

  Record.defrecord(:rsm_in,
    max:        :undefined,
    direction:  :undefined,
    id:         :undefined,
    index:      :undefined,
    reverse:    false)

  Record.defrecord(:rsm_out,
    count:  0,
    index:  :undefined,
    first:  :undefined,
    last:   :undefined)

  @type sorting :: {:asc | :desc, atom}
  @type direction :: :before | :aft
  @type rsm_in :: record(:rsm_in,
                         max:       non_neg_integer | :undefined,
                         direction: direction | :undefined,
                         id:        binary | integer | :undefined,
                         index:     non_neg_integer | :undefined,
                         reverse:   boolean)

  @type rsm_out :: record(:rsm_out,
                          count: non_neg_integer,
                          index: non_neg_integer | :undefined,
                          first: binary | :undefined,
                          last:  binary | :undefined)

  defmacro __using__(_) do
    quote do
      alias unquote(__MODULE__)
      import unquote(__MODULE__), only: :macros
    end
  end

  @doc """
  `rsm_query/4`, takes four parameters:

  * `rsm_in` is an RSM record with the parameters for the query.
  * `queryable` is an Ecto Queryable.t struct which describes how to retrieve
    the full set of elements upon which the query is to act (for example, all
    bots belonging to a given user).
  * `key_field` is the name of the field upon which the RSM IDs are based.
  * `sorting` is of the form `{:asc | :desc, sort_field}` and describes how
    objects within the entire result set are to be sorted.

  The function will compose `queryable` with the extra query options necessary
  to retrive the RSM set requested, run the query, and return
  `{objects, rsm_out}`.
  """
  @spec rsm_query(rsm_in, Ecto.Queryable.t, atom, sorting) ::
    {[struct], rsm_out}
  def rsm_query(rsm_in, queryable, key_field, sorting) do
    rsm_in = sanitise_rsm_in(rsm_in)
    dir = direction(rsm_in(rsm_in, :direction))
    records = get_records(rsm_in, queryable, key_field, dir, sorting)
    count = get_count(queryable, key_field)
    index = get_index(records, queryable, sorting)

    {maybe_reverse_result(records, rsm_in(rsm_in, :reverse)),
     rsm_out(count: count,
             index: index,
             first: to_rsm_id(get_first(records, key_field)),
             last:  to_rsm_id(get_last(records, key_field)))
    }
  end

  # index and id should never both be present - if they are, drop index
  defp sanitise_rsm_in(rsm_in) do
    if rsm_in(rsm_in, :index) != :undefined and
       rsm_in(rsm_in, :id) != :undefined do
      rsm_in(rsm_in, index: :undefined)
    else
      rsm_in
    end
  end

  defp get_records(rsm_in, queryable, key_field, dir, sorting) do
    queryable
    |> maybe_join_clause(rsm_in(rsm_in, :id), key_field, dir, sorting)
    |> maybe_limit(rsm_in(rsm_in, :max))
    |> maybe_offset(rsm_in(rsm_in, :index))
    |> order(dir, sorting)
    |> Repo.all()
    |> maybe_reverse_dir(dir)
  end

  defp get_count(queryable, key_field) do
    queryable
    |> exclude(:preload)
    |> exclude(:select)
    |> select([r, ...], count(field(r, ^key_field)))
    |> Repo.one!()
  end

  defp get_index([], _, _), do: :undefined
  defp get_index([first | _], queryable, {_, sort_field} = sorting) do
    pivot = Map.get(first, sort_field)

    queryable
    |> exclude(:preload)
    |> exclude(:select)
    |> select([r], count(field(r, ^sort_field)))
    |> index_where(pivot, sorting)
    |> Repo.one!()
  end

  defp index_where(queryable, key, {:asc, sort_field}) do
    queryable
    |> where([r, ...], field(r, ^sort_field) < ^key)
  end
  defp index_where(queryable, key, {:desc, sort_field}) do
    queryable
    |> where([r, ...], field(r, ^sort_field) > ^key)
  end


  defp direction(:undefined), do: :aft
  defp direction(dir), do: dir

  defp maybe_limit(query, :undefined), do: query
  defp maybe_limit(query, max), do: from [h, ...] in query, limit: ^max

  defp maybe_offset(query, :undefined), do: query
  defp maybe_offset(query, index), do: from [h, ...] in query, offset: ^index

  defp maybe_join_clause(queryable, "", _, _, _), do: queryable
  defp maybe_join_clause(queryable, :undefined, _, _, _), do: queryable
  defp maybe_join_clause(queryable, key, key_field, dir, {sort_order, sort_field})
  when is_order_forward(dir, sort_order) do
    subquery = queryable |> exclude(:preload)
    queryable
    |> join(:inner, [r, ...], p in subquery(subquery),
            field(p, ^key_field) == ^key and
            field(r, ^sort_field) > field(p, ^sort_field))
  end
  defp maybe_join_clause(queryable, key, key_field, _, {_, sort_field}) do
    subquery = queryable |> exclude(:preload)
    queryable
    |> join(:inner, [r, ...], p in subquery(subquery),
            field(p, ^key_field) == ^key and
            field(r, ^sort_field) < field(p, ^sort_field))
  end

  defp order(query, dir, {sort_order, sort_field})
  when is_order_forward(dir, sort_order) do
    from [r, ...] in query, order_by: ^sort_field
  end
  defp order(query, _, {_, sort_field}) do
    from [r, ...] in query, order_by: [desc: ^sort_field]
  end

  defp maybe_reverse_dir(records, :aft), do: records
  defp maybe_reverse_dir(records, :before), do: Enum.reverse(records)

  defp maybe_reverse_result(records, false), do: records
  defp maybe_reverse_result(records, true), do: Enum.reverse(records)

  defp get_first([], _), do: :undefined
  defp get_first([r | _], key_field), do: Map.get(r, key_field)
  defp get_last([], _), do: :undefined
  defp get_last(recs, key_field), do: recs |> Enum.at(-1) |> Map.get(key_field)

  defp to_rsm_id(:undefined), do: :undefined
  defp to_rsm_id(b) when is_binary(b), do: b
  defp to_rsm_id(i) when is_integer(i), do: Integer.to_string(i)
  defp to_rsm_id(f) when is_float(f), do: Float.to_string(f)
  defp to_rsm_id(%DateTime{} = d), do: Timestamp.to_string(d)
end
