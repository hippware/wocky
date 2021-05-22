defmodule WockyAPI.Types.GeoJSON do
  @moduledoc """
  Absinthe scalar types for GeoJSON
  """

  use Absinthe.Schema.Notation

  alias Absinthe.Blueprint.Input.Null
  alias Absinthe.Blueprint.Input.String, as: InputString

  scalar :geojson_coordinates, name: "GeoJSONCoordinates" do
    serialize fn
      m when is_list(m) -> Poison.encode!(m)
      _ -> throw("Invalid type for GeoJSONCoordinates - must be a list")
    end

    parse fn
      %InputString{value: value} -> Poison.decode(value)
      %Null{} -> {:ok, nil}
      _ -> :error
    end
  end

  scalar :json_object, name: "JSONObject" do
    serialize fn
      m when is_map(m) -> Poison.encode!(m)
      _ -> throw("Invalid type for JSONObject - must be map")
    end

    parse fn
      %InputString{value: value} -> Poison.decode(value)
      %Null{} -> {:ok, nil}
      _ -> :error
    end
  end
end
