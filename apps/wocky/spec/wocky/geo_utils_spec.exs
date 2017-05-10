defmodule Wocky.GeoUtilsSpec do
  use ESpec, async: true

  import Wocky.GeoUtils

  describe "to_degrees/1" do
    it do: to_degrees("1")    |> should(eql 1.0)
    it do: to_degrees("1.0")  |> should(eql 1.0)
    it do: to_degrees(1)      |> should(eql 1.0)
    it do: to_degrees(1.0)    |> should(eql 1.0)
    it do: to_degrees("-1")   |> should(eql -1.0)
    it do: to_degrees("-1.0") |> should(eql -1.0)
    it do: to_degrees(-1)     |> should(eql -1.0)
    it do: to_degrees(-1.0)   |> should(eql -1.0)
    it do: fn -> to_degrees("blarg") end |> should(raise_exception ArgumentError)
    it do: fn -> to_degrees(:blarg) end  |> should(raise_exception ArgumentError)
  end

  describe "normalize_latitude/1" do
    it do: normalize_latitude(1.0) |> should(eql 1.0)
    it do: normalize_latitude(91.0) |> should(eql -89.0)
    it do: normalize_latitude(-91.0) |> should(eql 89.0)
  end

  describe "normalize_longitude/1" do
    it do: normalize_longitude(1.0) |> should(eql 1.0)
    it do: normalize_longitude(181.0) |> should(eql -179.0)
    it do: normalize_longitude(-181.0) |> should(eql 179.0)
  end
end
