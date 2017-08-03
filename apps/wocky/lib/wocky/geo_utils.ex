defmodule Wocky.GeoUtils do
  alias Geo.Point

  @moduledoc "Geographic utilities for Wocky"

  @doc "Convert a string or integer to a float"
  @spec to_degrees(binary | integer | float) :: float | no_return
  def to_degrees(string) when is_binary(string) do
    case Float.parse(string) do
      {float, _} -> float
      :error   -> raise ArgumentError
    end
  end
  def to_degrees(integer) when is_integer(integer), do: integer * 1.0
  def to_degrees(float) when is_float(float), do: float
  def to_degrees(_), do: raise ArgumentError

  def point(lon, lat) do
    %Point{coordinates: {lon, lat}, srid: 4326}
  end

  @doc "Normalize latitude and logitude to the range [-90,90], (-180, 180]"
  @spec normalize_lat_lon(float, float) :: {float, float}
  def normalize_lat_lon(lat, lon) do
    quadrant = round(fmod(Float.floor(abs(lat) / 90), 4))
    pole =
      if lat > 0 do 90
      else -90
      end
    offset = fmod(lat, 90)

    {nlat, ilon} =
      case quadrant do
        0 -> {offset, lon}
        1 -> {pole - offset, lon + 180}
        2 -> {-offset, lon + 180}
        3 -> {-pole + offset, lon}
      end

    nlon =
      if (ilon > 180) || (ilon <= -180) do
        ilon - Float.floor((ilon + 180) / 360) * 360
      else
        ilon
      end
    {nlat, nlon}
  end

  defp fmod(a, b) do
    multiple = trunc(a / b)
    a - (multiple * b)
  end
end
