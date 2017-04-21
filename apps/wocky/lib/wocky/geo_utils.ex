defmodule Wocky.GeoUtils do
  @moduledoc "Geographic utilities for Wocky"

  @type degrees :: binary | float

  @doc "Normalize longitude to the range [-180,180]"
  @spec normalize_longitude(degrees) :: degrees
  def normalize_longitude(degrees) when is_binary(degrees) do
    # Note that this will throw an error if degrees is an integer
    degrees |> String.to_float |> normalize_longitude()
  end
  def normalize_longitude(degrees) when degrees < -180 do
    normalize_longitude(degrees + (2 * 180))
  end
  def normalize_longitude(degrees) when degrees > 180 do
    normalize_longitude(degrees - (2 * 180))
  end
  def normalize_longitude(degrees) do
    degrees
  end

  @doc "Normalize latitude to the range [-90,90]"
  @spec normalize_latitude(degrees) :: degrees
  def normalize_latitude(degrees) when is_binary(degrees) do
    # Note that this will throw an error if degrees is an integer
    degrees |> String.to_float |> normalize_latitude()
  end
  def normalize_latitude(degrees) when degrees < -90 do
    normalize_latitude(degrees + (2 * 90))
  end
  def normalize_latitude(degrees) when degrees > 90 do
    normalize_latitude(degrees - (2 * 90))
  end
  def normalize_latitude(degrees) do
    degrees
  end
end
