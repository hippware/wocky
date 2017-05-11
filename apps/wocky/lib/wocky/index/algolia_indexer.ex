defmodule Wocky.Index.AlgoliaIndexer do
  @moduledoc """
  Implementes the `Index` behavior for Algolia.
  """

  require Logger

  @behaviour Wocky.Index

  def init do
    Logger.info("Algolia indexer enabled")
  end

  def update_object(index, id, map) do
    object = map |> set_object_id(id) |> with_geoloc
    Algolia.partial_update_objects(index, [object])
    :ok
  end

  def delete_object(index, id) do
    {:ok, _} = Algolia.delete_object(index, id)
    :ok
  end

  def geosearch(index, lat, lon) do
    {:ok, result} =
      Algolia.search(index, <<>>, %{
          aroundLatLng: "#{lat},#{lon}",
          getRankingInfo: true
      })

    bots = Enum.map(result["hits"], &object_to_bot/1)
    {:ok, bots}
  end

  defp set_object_id(map, id) do
    Map.put(map, "objectID", id)
  end

  defp with_geoloc(%{lat: lat, lon: lon} = data) do
    Map.put(data, "_geoloc", %{lat: lat, lng: lon})
  end
  defp with_geoloc(data), do: data

  defp object_to_bot(obj) do
    %{
      id: obj["objectID"],
      server: obj["server"],
      owner: obj["owner"],
      title: obj["title"],
      image: obj["image"],
      lat: obj["lat"],
      lon: obj["lon"],
      radius: obj["radius"],
      distance: obj["_rankingInfo"]["geoDistance"] * 1000 # millimeters
    }
  end
end
