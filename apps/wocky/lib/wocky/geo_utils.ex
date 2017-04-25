defmodule Wocky.GeoUtils do
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

  @doc "Normalize longitude to the range [-180,180]"
  @spec normalize_longitude(float) :: float
  def normalize_longitude(degrees) do
    normalize_degrees(degrees, -180, 180)
  end

  @doc "Normalize latitude to the range [-90,90]"
  @spec normalize_latitude(float) :: float
  def normalize_latitude(degrees) do
    normalize_degrees(degrees, -90, 90)
  end

  defp normalize_degrees(degrees, lower, upper) when degrees < lower do
    normalize_degrees(degrees + (2 * upper), lower, upper)
  end
  defp normalize_degrees(degrees, lower, upper) when degrees > upper do
    normalize_degrees(degrees - (2 * upper), lower, upper)
  end
  defp normalize_degrees(degrees, _lower, _upper) do
    degrees
  end
end
