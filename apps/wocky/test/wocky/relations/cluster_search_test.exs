defmodule Wocky.Bots.ClusterSearchTest do
  use Wocky.DataCase, async: true

  alias Wocky.Bots.Bot
  alias Wocky.GeoUtils
  alias Wocky.Relations
  alias Wocky.Relations.Cluster
  alias Wocky.Relations.ClusterSearch
  alias Wocky.Repo.Factory

  @third 1.0 / 3.0
  describe "search/5" do
    test "simple search" do
      {:ok, user: user, bots: bots} =
        [
          {1.5, 1.5},
          {0.1, 0.1},
          {0.2, 0.2},
          {0.3, 0.3},
          {0.67, 0.67},
          {0.68, 0.68}
        ]
        |> insert_locs()

      a = GeoUtils.point(0.0, 0.0)
      b = GeoUtils.point(2.0, 2.0)

      results = ClusterSearch.search(a, b, 3, 3, user)

      assert length(results) == 3

      expected_id = hd(bots).id

      assert Enum.any?(
               results,
               fn
                 %Bot{id: ^expected_id} -> true
                 %Cluster{} -> false
               end
             )

      assert_has_clusters(results, [{3, @third, @third}, {2, 1.0, 1.0}])
    end

    test "search across 180th meridian" do
      {:ok, user: user, bots: bots} =
        [
          {0, 179.9},
          {0, 179.3},
          {0, 179.4},
          {0, 179.5},
          {0, -179.3},
          {0, -179.4},
          {0, 178.9}
        ]
        |> insert_locs()

      a = GeoUtils.point(1.0, 179.0)
      b = GeoUtils.point(-1.0, -179.0)

      results = ClusterSearch.search(a, b, 3, 3, user)

      assert length(results) == 3

      expected_id = hd(bots).id

      assert Enum.any?(
               results,
               fn
                 %Bot{id: ^expected_id} -> true
                 %Cluster{} -> false
               end
             )

      assert_has_clusters(results, [
        {3, 0, 179 + @third},
        {2, 0, -179 - @third}
      ])
    end
  end

  defp insert_locs(locations) do
    u = Factory.insert(:user)

    b =
      Enum.map(locations, fn {lat, lon} ->
        l = GeoUtils.point(lat, lon)
        Factory.insert(:bot, location: l, user: u)
      end)

    Enum.each(b, &Relations.subscribe(&1, u))

    {:ok, user: u, bots: b}
  end

  defp assert_has_clusters(results, cluster_data) do
    c =
      cluster_data
      |> Enum.map(fn {c, lat, lon} ->
        %Cluster{count: c, location: GeoUtils.point(lat, lon)}
      end)
      |> Enum.sort()

    results
    |> Enum.filter(fn
      %Cluster{} -> true
      %Bot{} -> false
    end)
    |> Enum.sort()
    |> Enum.zip(c)
    |> Enum.each(&clusters_match/1)
  end

  defp clusters_match({c1, c2}) do
    assert c1.count == c2.count

    assert_in_delta(
      GeoUtils.get_lat(c1.location),
      GeoUtils.get_lat(c2.location),
      0.0000001
    )

    assert_in_delta(
      GeoUtils.get_lon(c1.location),
      GeoUtils.get_lon(c2.location),
      0.0000001
    )
  end
end
