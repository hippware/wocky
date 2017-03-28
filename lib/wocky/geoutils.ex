defmodule Wocky.GeoUtils do
  @moduledoc "Geographic utilities for Wocky"

  @type degrees :: integer | float

  @doc "Normalise degrees to the range [-180,180]"
  @spec normalize_degrees(degrees) :: degrees
  def normalize_degrees(degrees) when degrees < -180 do
    normalize_degrees(degrees + 2 * 180)
  end
  def normalize_degrees(degrees) when degrees > 180 do
    normalize_degrees(degrees - 2 * 180)
  end
  def normalize_degrees(degrees) do
    degrees
  end

end
