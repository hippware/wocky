defmodule Wocky.Bot.Geosearch.Limits do
  @moduledoc """
  Structure describing various limits for explore-nearby query
  """

  defstruct [:radius, :count, :time]
end

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
  alias Geo.Point
  alias Wocky.Bot
  alias Wocky.Bot.Geosearch.Limits
  alias Wocky.GeoUtils
  alias Wocky.Repo
  alias Wocky.User

  @type explore_end_message ::
        :no_more_results | :result_limit_reached | :max_explore_time
  @type explore_callback :: ((Bot | explore_end_message) -> any)

  @default_max_explored_bots 1_000_000
  @default_explore_timeout 60_000

  @spec user_distance_query(float, float, User.id, User.id, RSMHelper.rsm_in)
  :: {[%Bot{}], RSMHelper.rsm_out}
  def user_distance_query(lat, lon, user_id, owner_id, rsm_in \\ rsm_in()),
    do: distance_query(lat, lon,
     &where_owner_and_visible(&1, user_id, owner_id), rsm_in)

  @spec subscribed_distance_query(float, float, User.id, RSMHelper.rsm_in)
  :: {[%Bot{}], RSMHelper.rsm_out}
  def subscribed_distance_query(lat, lon, user_id, rsm_in \\ rsm_in()),
    do: distance_query(lat, lon,
     &where_subscribed(&1, user_id), rsm_in)

  defp distance_query(lat, lon, where_clause, rsm_in) do
    limit = rsm_in(rsm_in, :max)
    offset = rsm_in(rsm_in, :index)
    pivot = dump_uuid(rsm_in(rsm_in, :id))
    direction = rsm_in(rsm_in, :direction)
    reverse = rsm_in(rsm_in, :reverse)
    point = GeoUtils.point(lat, lon)

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
    do: do_get_all(lat, lon,
     &where_owner_and_visible(&1, user_id, owner_id))

  def get_all(lat, lon, user_id),
    do: do_get_all(lat, lon, &where_searchable(&1, user_id))

  defp do_get_all(lat, lon, where_clause) do
    point = GeoUtils.point(lat, lon)
    {query_str, params} =
      point
      |> fields()
      |> where_clause.()
      |> order(point, :aft)
      |> finalize_query()

    Repo
    |> SQL.query!(query_str, params)
    |> results_to_bots()
  end

  defp fields(point) do
    {~s|SELECT bot.*, ST_Distance(bot.location, #{p(1, [])})| <>
      ~s|AS "distance" FROM bots AS bot|,
     [point]}
  end

  defp count, do: {"SELECT count(id) FROM bots AS bot", []}

  defp where_owner_and_visible({str, params}, user_id, owner_id) do
    {str <>
      " WHERE bot.user_id = #{p(1, params)}" <>
       " AND is_visible(#{p(2, params)}, bot)",
      [dump_uuid(user_id), dump_uuid(owner_id) | params]}
  end

  defp where_subscribed({str, params}, user_id) do
    {str <>
     " INNER JOIN bot_subscriptions AS bot_subscription " <>
      " ON bot.id = bot_subscription.bot_id" <>
      " WHERE bot_subscription.user_id = #{p(1, params)}" <>
      " AND is_visible(#{p(1, params)}, bot)",
      [dump_uuid(user_id) | params]}
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

  defp order({str, params}, point, direction) do
    {str <>
      ~s| ORDER BY| <>
      ~s| ST_Distance(bot.location, #{p(1, params)}, true)| <>
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

  ### Explore nearby

  @spec explore_nearby(Point.t, float, User.t,
                       non_neg_integer, explore_callback) :: :ok
  def explore_nearby(point, radius, user, max, fun) do
    max_explore_time = Confex.get_env(:wocky, :max_explore_time,
                                      @default_explore_timeout)
    max_explored_bots = Confex.get_env(:wocky, :max_explored_bots,
                                       @default_max_explored_bots)

    query_str =
    """
    DECLARE explore_nearby CURSOR FOR
    SELECT *, ST_Distance(bot.location, $1) AS "distance" FROM
      (SELECT * FROM bots ORDER BY location <-> $1 LIMIT $2)
    AS bot
    WHERE is_searchable($3, bot)
    """
    params = [point, max_explored_bots, dump_uuid(user.id)]
    limits = %Limits{radius: radius, count: max, time: max_explore_time}

    Repo.transaction(
      fn() ->
        start_explore(query_str, params, fun, limits)
      end,
      timeout: :infinity)
    :ok
  end

  defp start_explore(query_str, params, fun, limits) do
    start_time = :erlang.monotonic_time()
    SQL.query!(Repo, query_str, params)
    fetch_results(fun, start_time, limits, 0)
  end

  defp fetch_results(fun, start_time,
                     %Limits{radius: radius} = limits, count) do
    results = SQL.query!(Repo, "FETCH NEXT explore_nearby")
    case results.num_rows do
      0 ->
        fun.(:no_more_results)
      _ ->
        bot = results |> results_to_bots() |> hd
        if bot.distance > radius do
          fun.(:no_more_results)
        else
          fun.(bot)
          maybe_fetch_more(fun, start_time, limits, count + 1)
        end
    end
  end

  defp maybe_fetch_more(fun, _start_time, %Limits{count: count}, count) do
    fun.(:result_limit_reached)
  end
  defp maybe_fetch_more(fun, start_time,
                        %Limits{time: timeout} = limits, count) do
    elapsed = :erlang.convert_time_unit(
      :erlang.monotonic_time() - start_time,
      :native, :millisecond)
    if elapsed < timeout do
      fetch_results(fun, start_time, limits, count)
    else
      fun.(:max_explore_time)
    end
  end
end
