defmodule Wocky.GeoUtilsTest do
  use ExUnit.Case, async: true

  import Wocky.GeoUtils

  test "to_degrees/1" do
    assert to_degrees("1") == 1.0
    assert to_degrees("1.0") == 1.0
    assert to_degrees(1) == 1.0
    assert to_degrees(1.0) == 1.0
    assert to_degrees("-1") == -1.0
    assert to_degrees("-1.0") == -1.0
    assert to_degrees(-1) == -1.0
    assert to_degrees(-1.0) == -1.0

    assert_raise ArgumentError, fn -> to_degrees("blarg") end
    assert_raise ArgumentError, fn -> to_degrees(:blarg) end
  end

  test "normalize_lat_lon/1" do
    assert normalize_lat_lon(1.0, 1.0) == {1.0, 1.0}
    assert normalize_lat_lon(91.0, 1.0) == {89.0, -179.0}
    assert normalize_lat_lon(181.0, 1.0) == {-1.0, -179.0}
    assert normalize_lat_lon(-91.0, 1.0) == {-89.0, -179.0}
    assert normalize_lat_lon(380.0, 1.0) == {20.0, 1.0}
    assert normalize_lat_lon(1.0, 181.0) == {1.0, -179.0}
    assert normalize_lat_lon(1.0, 181.0) == {1.0, -179.0}
    assert normalize_lat_lon(290.0, 1.0) == {-70.0, 1.0}

    # Check that very large numbers don't make it choke:
    assert normalize_lat_lon(1.797e+308, -1.6e+308) == {0.0, 0.0}
  end
end
