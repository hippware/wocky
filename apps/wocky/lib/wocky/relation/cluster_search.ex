defmodule Wocky.Relation.ClusterSearch do
  @moduledoc """
  Handler module for conducting clustering location-based bot searches
  """

  alias Ecto.Adapters.SQL
  alias Ecto.UUID
  alias Geo.Point
  alias Wocky.Account.User
  alias Wocky.GeoUtils
  alias Wocky.POI.Bot
  alias Wocky.Relation.Cluster
  alias Wocky.Repo

  @spec search(Point.t(), Point.t(), pos_integer, pos_integer, User.t()) :: [
          Bot.t() | Cluster.t()
        ]
  def search(point_a, point_b, lat_divs, lon_divs, user) do
    lat_a = GeoUtils.get_lat(point_a)
    lon_a = GeoUtils.get_lon(point_a)
    lat_b = GeoUtils.get_lat(point_b)
    lon_b = GeoUtils.get_lon(point_b)

    lat_grid = abs((lat_b - lat_a) / lat_divs)

    # If the search rectangle crosses the 180th meridian we need to normalise
    # it around before calculating the grid size
    lon_angle = abs(lon_b - lon_a)
    norm_lon_angle = min(lon_angle, 360 - lon_angle)
    lon_grid = norm_lon_angle / lon_divs

    lat_origin = lat_a + lat_grid / 2.0
    lon_origin = lon_a + lon_grid / 2.0

    query = """
    -- Select relevant bots in the search area and add a column with their
    -- location snapped to a grid of potential cluster points
    WITH snapped AS
      (SELECT
          bots.*, ST_SnapToGrid(location::geometry, $1, $2, $3, $4) snapped_loc
        FROM bots JOIN bot_subscriptions ON bots.id = bot_subscriptions.bot_id
        WHERE bot_subscriptions.user_id = $5
        AND bots.location && ST_MakeEnvelope($6, $7, $8, $9, 4326)::geography),

    -- Count bots at each cluster point
    counts AS
      (SELECT count(*) loc_count, snapped_loc
        FROM snapped GROUP BY snapped_loc),

    -- Merge the count value for the relevant cluster point into each bot
    aggregate AS
      (SELECT * FROM snapped
       JOIN counts ON snapped.snapped_loc = counts.snapped_loc)

    -- Select bots where they are the only one near their cluster point
    SELECT #{field_list("aggregate.")}, Null loc_count, Null snapped_loc
      FROM aggregate WHERE loc_count = 1

    UNION

    -- Select clusters where there is more than one bot at the cluster point
    SELECT #{null_field_list()}, loc_count, snapped_loc
      FROM counts WHERE loc_count > 1
    """

    {:ok, user_id} = UUID.dump(user.id)

    result =
      SQL.query!(Repo, query, [
        lon_origin,
        lat_origin,
        lon_grid,
        lat_grid,
        user_id,
        lon_a,
        lat_a,
        lon_b,
        lat_b
      ])

    cols = Enum.map(result.columns, &String.to_atom/1)

    Enum.map(result.rows, fn r ->
      cols
      |> Enum.zip(r)
      |> Enum.into(%{})
      |> map_result()
    end)
  end

  defp fields, do: Bot.__schema__(:fields)

  defp field_list(table_prefix) do
    fields()
    |> Enum.map(&to_string/1)
    |> Enum.map(&(table_prefix <> &1))
    |> Enum.join(", ")
  end

  defp null_field_list do
    "Null"
    |> List.duplicate(length(fields()))
    |> Enum.join(", ")
  end

  defp map_result(%{id: nil, loc_count: c, snapped_loc: l}),
    do: %Cluster{count: c, location: l}

  defp map_result(r),
    do: Repo.load(Bot, r)
end
