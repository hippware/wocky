defmodule Wocky.TROS.AlgoliaIndexerTest do
  use ExUnit.Case, async: false
  use ExVCR.Mock, adapter: ExVCR.Adapter.Hackney

  alias ExVCR.Setting
  alias Wocky.GeoUtils

  import Wocky.Index.AlgoliaIndexer

  @index "test_wocky_bots"
  @server "localhost"
  @object_id1 "92ec75ce-5c9c-11e7-a6fc-6bc2ef6cf59f"
  @object_id2 "9992ad9e-5c9c-11e7-be21-8bd16ebc6dce"

  setup do
    Application.put_env(:algolia, :application_id, "765J3YW5XN")
    Application.put_env(:algolia, :api_key, "ffd47f99438c5dfe778a307d9a58e29a")
  end

  test "update_object/3" do
    use_cassette "update_object3", match_requests_on: [:request_body] do
      :ok = update_object(@index, @object_id1,
                          %{"server" => @server,
                            "title" => "title1",
                            "image" => "image1",
                            "location" => GeoUtils.point(1.0, 2.0),
                            "radius" => 1000,
                            "public" => true})
      :ok = update_object(@index, @object_id2,
                          %{"server" => @server,
                            "title" => "title2",
                            "image" => "image2",
                            "location" => GeoUtils.point(5.0, 6.0),
                            "radius" => 1000,
                            "public" => true})
    end
  end

  test "geosearch/3" do
    use_cassette "geosearch3" do
      if recording?("geosearch3"), do: :timer.sleep(:timer.seconds(15))
      {:ok, bots} = geosearch(@index, 1.0, 2.0)
      assert length(bots) == 2
      [bot, bot2] = bots
      assert bot[:id] == @object_id1
      assert bot[:distance] == 0
      assert bot[:server] == @server
      assert bot2[:id] == @object_id2
      assert bot2[:distance] != nil
      assert bot2[:distance] > 0
    end
  end

  test "delete_object/2" do
    use_cassette "delete_object2" do
      :ok = delete_object(@index, @object_id2)
      if recording?("delete_object2"), do: :timer.sleep(:timer.seconds(15))
      {:ok, bots} = geosearch(@index, 1.0, 2.0)
      assert length(bots) == 1
      assert hd(bots)[:id] == @object_id1
    end
  end

  defp recording?(cassette) do
    dir = Setting.get(:cassette_library_dir)
    not File.exists?("#{dir}/#{cassette}.json")
  end

end
