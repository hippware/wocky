defmodule Wocky.RSMHelper.Guard do
  defmacro order_forward(dir, sort_order) do
    quote do
      (unquote(dir) == :aft and unquote(sort_order) == :asc) or
      (unquote(dir) == :before and unquote(sort_order) == :desc)
    end
  end
end

defmodule Wocky.RSMHelper do
  @moduledoc ""

  use Wocky.Repo.Model

  require Record
  import Wocky.RSMHelper.Guard

  # FIXME: make this explicit so we're not relying on ejabberd
  @rsm_hdr "ejabberd/include/jlib.hrl"

  Record.defrecord :rsm_in,  Record.extract(:rsm_in, from_lib: @rsm_hdr)
  Record.defrecord :rsm_out, Record.extract(:rsm_out, from_lib: @rsm_hdr)

  @type rsm_in :: record(:rsm_in)
  @type rsm_out :: record(:rsm_out)
  @type sorting :: {:asc | :desc, atom}

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
             first: get_first(records, key_field),
             last: get_last(records, key_field))
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
    |> select([r], count(field(r, ^key_field)))
    |> Repo.one!()
  end

  defp get_index([], _, _), do: :undefined
  defp get_index([first | _], queryable, {sort_order, sort_field}) do
    pivot = Map.get(first, sort_field)

    queryable
    |> select([r], count(field(r, ^sort_field)))
    |> index_where(sort_field, pivot, sort_order)
    |> Repo.one!()
  end

  defp index_where(queryable, sort_field, key, :asc) do
    queryable
    |> where([r], field(r, ^sort_field) < ^key)
  end
  defp index_where(queryable, sort_field, key, :desc) do
    queryable
    |> where([r], field(r, ^sort_field) > ^key)
  end


  defp direction(:undefined), do: :aft
  defp direction(dir), do: dir

  defp maybe_limit(query, :undefined), do: query
  defp maybe_limit(query, max), do: from h in query, limit: ^max

  defp maybe_offset(query, :undefined), do: query
  defp maybe_offset(query, index), do: from h in query, offset: ^index

  defp maybe_join_clause(queryable, "", _, _, _), do: queryable
  defp maybe_join_clause(queryable, :undefined, _, _, _), do: queryable
  defp maybe_join_clause(queryable, key, key_field, dir, {sort_order, sort_field})
  when order_forward(dir, sort_order) do
    queryable
    |> join(:inner, [r], p in ^queryable,
            field(p, ^key_field) == ^key and
            field(r, ^sort_field) > field(p, ^sort_field))
  end
  defp maybe_join_clause(queryable, key, key_field, _, {_, sort_field}) do
    queryable
    |> join(:inner, [r], p in ^queryable,
            field(p, ^key_field) == ^key and
            field(r, ^sort_field) < field(p, ^sort_field))
  end

  defp order(query, dir, {sort_order, sort_field})
  when order_forward(dir, sort_order) do
    from r in query, order_by: ^sort_field
  end
  defp order(query, _, {_, sort_field}) do
    from r in query, order_by: [desc: ^sort_field]
  end

  defp maybe_reverse_dir(records, :aft), do: records
  defp maybe_reverse_dir(records, :before), do: Enum.reverse(records)

  defp maybe_reverse_result(records, false), do: records
  defp maybe_reverse_result(records, true), do: Enum.reverse(records)

  defp get_first([], _), do: :undefined
  defp get_first([r | _], key_field), do: Map.get(r, key_field)
  defp get_last([], _), do: :undefined
  defp get_last(recs, key_field), do: recs |> Enum.at(-1) |> Map.get(key_field)
end
