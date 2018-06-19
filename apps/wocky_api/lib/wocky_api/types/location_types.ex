defmodule WockyAPI.Schema.LocationTypes do
  @moduledoc """
  Absinthe types for wocky user
  """

  use WockyAPI.Schema.Notation

  @desc "A point on the globe"
  input_object :point do
    @desc "Latitude in degrees"
    field :lat, non_null(:float)

    @desc "Longitude in degrees"
    field :lon, non_null(:float)
  end
end
