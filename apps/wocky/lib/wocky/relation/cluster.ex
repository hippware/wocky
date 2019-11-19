defmodule Wocky.Relation.Cluster do
  @moduledoc """
  Struct representing a group of bots around an area
  """

  defstruct [
    :count,
    :location
  ]

  @type t :: %__MODULE__{
          count: integer(),
          location: Geo.Point.t()
        }
end
