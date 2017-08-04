defmodule Wocky.Bot.Geosearch do
  @moduledoc """
  In an ideal world, this module wouldn't have to exist. However Ecto has some
  limitations at the moment that make query composition not quite powerful
  enough for us to throw together geosearch queries and feed them to RSMHelper.
  In particular see
  https://groups.google.com/forum/#!msg/elixir-ecto/Yr1c08C5s0g/kZApI3W5BwAJ
  and
  https://groups.google.com/forum/#!msg/elixir-ecto/U6-inhux9-E/dCXet4KxBQAJ

  So as such we have this module which combines hand crafted SQL and some
  stored procedures to do RSM/geosearch queries.
  """
  use Wocky.RSMHelper

  alias Ecto.Adapters.SQL
  alias Ecto.UUID
  alias Wocky.Bot
  alias Wocky.GeoUtils
  alias Wocky.Repo

  @spec user_distance_query(float, float, User.id, User.id RSMHelper.rsm_in)
  :: {[%Bot{}], RSMHelper.rsm_out}
  def user_distance_query(lat, lon, user_id, owner_id, rsm_in \\ rsm_in()),
    do: distance_query(lat, lon, &where_visible(&1, user_id, owner_id), rsm_in)

  defp distance_query(lat, lon, where_clause, rsm_in) do
    limit = rsm_in(rsm_in, :max)
    offset = rsm_in(rsm_in, :index)
    pivot = dump_uuid(rsm_in(rsm_in, :id))
    direction = rsm_in(rsm_in, :direction)
    reverse = rsm_in(rsm_in, :reverse)
    point = GeoUtils.point(lon, lat)

    {query_str, params} =
      point
      |> fields()
      |> maybe_join(pivot, point, direction)
      |> where_clause.()
      |> order(point, direction)
      |> maybe_limit(limit)
      |> maybe_offset(offset)
      |> finalize_query()

    results =
      Repo
      |> SQL.query!(query_str, params)
      |> results_to_bots()
      |> fix_sorting(direction)

    count = get_count(where_clause)
    index = get_index(results, point, where_clause)
    {first, last} = get_first_last(results)

    {maybe_reverse(results, reverse),
      rsm_out(count: count, index: index, first: first, last: last)}
  end

  def get_all(lat, lon, user_id, owner_id),
    do: do_get_all(lat, lon, &where_visible(&1, user_id, owner_id), true)

  def get_all(lat, lon, user_id),
    do: do_get_all(lat, lon, &where_searchable(&1, user_id), false)

  defp do_get_all(lat, lon, where_clause, order_with_sphereoid) do
    point = GeoUtils.point(lon, lat)
    {query_str, params} =
      point
      |> fields()
      |> where_clause.()
      |> order(point, :aft, order_with_sphereoid)
      |> finalize_query()

    Repo
    |> SQL.query!(query_str, params)
    |> results_to_bots()
  end

  defp fields(point) do
    {~s|SELECT *, ST_Distance(bot.location, #{p(1, [])})| <>
      ~s|AS "distance" FROM bots AS bot|,
     [point]}
  end

  defp count, do: {"SELECT count(id) FROM bots AS bot", []}

  defp where_visible({str, params}, user_id, owner_id) do
    {str <>
      " WHERE bot.user_id = #{p(1, params)}" <>
       " AND is_visible(#{p(2, params)}, bot)",
      [dump_uuid(user_id), dump_uuid(owner_id) | params]}
  end

  defp where_searchable({str, params}, user_id) do
    {str <>
      " WHERE is_searchable(#{p(1, params)}, bot)",
      [dump_uuid(user_id) | params]}
  end

  defp index_where({str, params}, point, pivot_dist) do
    {str <> " AND ST_Distance(bot.location, #{p(1, params)}) < #{p(2, params)}",
      [pivot_dist, point | params]}
  end

  defp maybe_join(acc, :undefined, _, _), do: acc
  defp maybe_join(acc, pivot, point, dir) do
    acc
    |> add_str(" INNER JOIN")
    |> join_fields()
    |> join_on(pivot, point, dir)
  end

  defp join_fields(acc) do
    add_str(acc, " (SELECT id AS pivot_id, location " <>
                  "AS pivot_location FROM bots) AS pivot")
  end

  defp join_on({str, params}, pivot, point, dir) do
    {str <>
      ~s| ON (pivot.pivot_id = #{p(1, params)})| <>
      ~s| AND (ST_Distance(bot.location, #{p(2, params)})| <>
      ~s| #{dir(dir)} | <>
      ~s| ST_Distance(pivot.pivot_location, #{p(2, params)}))|,
        [point, pivot | params]}
  end

  defp dir(:before), do: "<"
  defp dir(_), do: ">"

  defp maybe_limit(acc, :undefined), do: acc
  defp maybe_limit({str, params}, max), do:
    {str <> " LIMIT #{p(1, params)}", [max | params]}

  defp maybe_offset(acc, :undefined), do: acc
  defp maybe_offset({str, params}, offset),
    do: {str <> " OFFSET #{p(1, params)}", [offset | params]}

  defp order({str, params}, point, direction, use_sphereoid \\ true) do
    {str <>
      ~s| ORDER BY| <>
      ~s| ST_Distance(bot.location, #{p(1, params)}, #{use_sphereoid})| <>
      ~s| #{maybe_desc(direction)}|,
     [point | params]}
  end

  defp maybe_desc(:before), do: "DESC"
  defp maybe_desc(_), do: ""

  defp p(index, params), do: "$#{length(params) + index}"

  defp add_str({str, params}, tail), do: {str <> tail, params}

  defp finalize_query({str, params}), do: {str, Enum.reverse(params)}

  ### Post-processing

  def results_to_bots(%{columns: columns, rows: rows}) do
    Enum.map(rows, &row_to_bot(&1, columns))
  end

  defp row_to_bot(row, columns) do
    map =
      columns
      |> Enum.zip(row)
      |> Enum.into(%{})

    Bot
    |> Repo.load(map)
    |> Map.put(:distance, Map.get(map, "distance"))
  end

  defp fix_sorting(rows, :before), do: Enum.reverse(rows)
  defp fix_sorting(rows, _), do: rows

  defp maybe_reverse(rows, true), do: Enum.reverse(rows)
  defp maybe_reverse(rows, false), do: rows

  defp get_count(where_clause) do
    {query_str, params} =
      count()
      |> where_clause.()
      |> finalize_query()

    results = SQL.query!(Repo, query_str, params)
    hd(hd(results.rows))
  end

  defp get_index([], _, _), do: :undefined
  defp get_index([first | _], point, where_clause) do
    {query_str, params} =
      count()
      |> where_clause.()
      |> index_where(point, first.distance)
      |> finalize_query()

    results = SQL.query!(Repo, query_str, params)
    hd(hd(results.rows))
  end

  defp get_first_last([]), do: {:undefined, :undefined}
  defp get_first_last(results) do
    {Enum.at(results, 0).id, Enum.at(results, -1).id}
  end

  defp dump_uuid(:undefined), do: :undefined
  defp dump_uuid(uuid), do: uuid |> UUID.dump |> elem(1)

  @spec explore_nearby(float, float, User.id, non_neg_integer) :: :ok
  def explore_nearby(lat, lon, user_id, fun, limit \\ 1000) do
    point = GeoUtils.point(lon, lat)
    query_str =
    """
    DECLARE explore_nearby CURSOR FOR
    SELECT *, ST_Distance(bot.location, $1) AS "distance" FROM bots AS bot
    WHERE is_searchable($2, bot)
    ORDER BY location <-> $1
    LIMIT $3
    """
    params = [point, dump_uuid(user_id), limit]

    Repo.transaction(fn() ->
      SQL.query!(Repo, query_str, params)
      fetch_results(fun) end)
  end

  def fetch_results(fun) do
    results = SQL.query!(Repo, "FETCH NEXT explore_nearby")
    case results.num_rows do
      0 ->
        fun.(:no_more_results)
        :ok
      _ ->
        fun.(results |> results_to_bots() |> hd)
        fetch_results(fun)
    end
  end
end
