defmodule WockyAPI.Schema.GeoJSONTypes do
  @moduledoc """
  Absinthe types for GeoJSON

  See https://tools.ietf.org/html/rfc7946
  """

  use WockyAPI.Schema.Notation

  enum :geojson_type do
    value :point
    value :multi_point
    value :line_string
    value :multi_line_string
    value :polygon
    value :multi_polygon
    value :geometry_collection
    value :feature
    value :feature_collection
  end

  enum :geojson_coordinate_system_type do
    value :name
    value :link
  end

  object :geojson_named_crs_properties do
    field :name, non_null(:string)
  end

  object :geojson_linked_crs_properties do
    field :href, non_null(:string)
    field :type, :string
  end

  union :geojson_crs_properties do
    types [
      :geojson_named_crs_properties,
      :geojson_linked_crs_properties
    ]
  end

  object :geojson_coordinate_reference_system do
    field :type, non_null(:string)
    field :properties, non_null(:geojson_crs_properties)
  end

  interface :geojson_interface do
    field :type, non_null(:geojson_type)
    field :crs, :geojson_coordinate_reference_system
    field :bbox, list_of(:float)

    resolve_type &resolve_geojson_type/2
  end

  interface :geojson_geometry_interface do
    field :type, non_null(:geojson_type)
    field :crs, :geojson_coordinate_reference_system
    field :bbox, list_of(:float)
    field :coordinates, :geojson_coordinates

    resolve_type &resolve_geojson_geometry_type/2
  end

  object :geojson_point do
    interfaces [:geojson_interface, :geojson_geometry_interface]
    import_fields :geojson_geometry_interface
  end

  object :geojson_multi_point do
    interfaces [:geojson_interface, :geojson_geometry_interface]
    import_fields :geojson_geometry_interface
  end

  object :geojson_line_string do
    interfaces [:geojson_interface, :geojson_geometry_interface]
    import_fields :geojson_geometry_interface
  end

  object :geojson_multi_line_string do
    interfaces [:geojson_interface, :geojson_geometry_interface]
    import_fields :geojson_geometry_interface
  end

  object :geojson_polygon do
    interfaces [:geojson_interface, :geojson_geometry_interface]
    import_fields :geojson_geometry_interface
  end

  object :geojson_multi_polygon do
    interfaces [:geojson_interface, :geojson_geometry_interface]
    import_fields :geojson_geometry_interface
  end

  object :geojson_geometry_collection do
    interfaces [:geojson_interface]
    import_fields :geojson_interface
    field :geometries, non_null(list_of(non_null(:geojson_geometry_interface)))
  end

  object :geojson_feature do
    interfaces [:geojson_interface]
    import_fields :geojson_interface

    field :geometry, :geojson_geometry_interface
    field :properties, :json_object
    field :id, :string
  end

  object :geojson_feature_collection do
    interfaces [:geojson_interface]
    import_fields :geojson_interface

    field :features, non_null(list_of(non_null(:geojson_feature)))
  end

  defp resolve_geojson_type(%{type: type} = object, info) do
    case type do
      "Feature" -> :geojson_feature
      "FeatureCollection" -> :geojson_feature_collection
      _ -> resolve_geojson_geometry_type(object, info)
    end
  end

  defp resolve_geojson_geometry_type(%{type: type}, _) do
    case type do
      "Point" -> :geojson_point
      "MultiPoint" -> :geojson_multi_point
      "LineString" -> :geojson_line_string
      "MultiLineString" -> :geojson_multi_line_string
      "Polygon" -> :geojson_polygon
      "MultiPolygon" -> :geojson_multi_polygon
      "GeometryCollection" -> :geojson_geometry_collection
      _ -> nil
    end
  end
end
