defmodule Wocky.Bots.Cluster do
  @moduledoc """
  Struct representing a group of bots around an area
  """

  defstruct [
    :count,
    :location
  ]

  @type t :: %Wocky.Bots.Cluster{
          count: integer,
          location: Geo.Point.t()
        }
end
