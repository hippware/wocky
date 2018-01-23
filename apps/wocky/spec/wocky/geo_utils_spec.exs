defmodule Wocky.GeoUtilsSpec do
  use ESpec, async: true

  import Wocky.GeoUtils

  describe "to_degrees/1" do
    it do: to_degrees("1") |> should(eql 1.0)
    it do: to_degrees("1.0") |> should(eql 1.0)
    it do: to_degrees(1) |> should(eql 1.0)
    it do: to_degrees(1.0) |> should(eql 1.0)
    it do: to_degrees("-1") |> should(eql -1.0)
    it do: to_degrees("-1.0") |> should(eql -1.0)
    it do: to_degrees(-1) |> should(eql -1.0)
    it do: to_degrees(-1.0) |> should(eql -1.0)

    it do:
         fn -> to_degrees("blarg") end |> should(raise_exception ArgumentError)

    it do: fn -> to_degrees(:blarg) end |> should(raise_exception ArgumentError)
  end

  describe "normalize_lat_lon/1" do
    it do: normalize_lat_lon(1.0, 1.0) |> should(eql {1.0, 1.0})
    it do: normalize_lat_lon(91.0, 1.0) |> should(eql {89.0, -179.0})
    it do: normalize_lat_lon(181.0, 1.0) |> should(eql {-1.0, -179.0})
    it do: normalize_lat_lon(-91.0, 1.0) |> should(eql {-89.0, -179.0})
    it do: normalize_lat_lon(380.0, 1.0) |> should(eql {20.0, 1.0})
    it do: normalize_lat_lon(1.0, 181.0) |> should(eql {1.0, -179.0})
    it do: normalize_lat_lon(1.0, 181.0) |> should(eql {1.0, -179.0})
    it do: normalize_lat_lon(290.0, 1.0) |> should(eql {-70.0, 1.0})
    # Check that very large numbers don't make it choke:
    it do:
         normalize_lat_lon(1.7976931348623157e+308, -1.6e+308)
         |> should(eql {0.0, 0.0})
  end
end
