defmodule Wocky.Index.AlgoliaIndexer do
  @moduledoc """
  Implementes the `Index` behavior for Algolia.
  """

  require Logger

  alias Geo.Point
  alias Wocky.GeoUtils

  @behaviour Wocky.Index

  def init do
    Logger.info("Algolia indexer enabled")
  end

  def update_object(index, id, map) do
    map
    |> set_object_id(id)
    |> with_geoloc
    |> do_update_object(index)
  end

  defp set_object_id(data, id) do
    Map.put(data, "objectID", id)
  end

  defp with_geoloc(%{"location" => %Point{coordinates: {lon, lat}}} = data) do
    data
    |> Map.delete("location")
    |> Map.put("lat", lat)
    |> Map.put("lon", lon)
    |> Map.put("_geoloc", %{"lat" => lat, "lng" => lon})
  end

  defp with_geoloc(data), do: data

  defp do_update_object(object, index) do
    Algolia.partial_update_objects(index, [object])
    :ok
  end

  def delete_object(index, id) do
    {:ok, _} = Algolia.delete_object(index, id)
    :ok
  end

  def geosearch(index, lat, lon) do
    {:ok, result} =
      Algolia.search(
        index,
        <<>>,
        aroundLatLng: "#{lat},#{lon}",
        getRankingInfo: true
      )

    bots = Enum.map(result["hits"], &object_to_bot/1)
    {:ok, bots}
  end

  defp object_to_bot(obj) do
    %{
      id: obj["objectID"],
      user_id: obj["user_id"],
      title: obj["title"],
      image: obj["image"],
      location: GeoUtils.point(obj["lat"], obj["lon"]),
      radius: obj["radius"],
      # millimeters
      distance: obj["_rankingInfo"]["geoDistance"] * 1000,
      public: obj["public"]
    }
  end
end
