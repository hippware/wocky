defmodule Wocky.Alerts do
  @moduledoc "Context for safety alerts."

  use Wocky.Context

  import Ecto.Query, only: [from: 2, subquery: 1]
  import Geo.PostGIS

  alias Wocky.Alerts.Alert
  alias Wocky.Alerts.Geometry

  @spec insert_geometry(map()) :: Repo.result(Geometry.t())
  def insert_geometry(params) do
    params
    |> Geometry.changeset()
    |> Repo.insert(
      on_conflict: {:replace, [:data, :geometry, :updated_at]},
      conflict_target: [:source, :source_id]
    )
  end

  @spec insert_alert(map()) :: Repo.result(Alert.t())
  def insert_alert(params) do
    geometry = lookup_geometry(params)

    params
    |> Map.put("geometry", geometry)
    |> Alert.changeset()
    |> Repo.insert(
      on_conflict: {
        :replace,
        [
          :expires_at,
          :title,
          :summary,
          :link,
          :geometry,
          :data,
          :updated_at,
          :imported
        ]
      },
      conflict_target: [:source, :source_id]
    )
  end

  defp lookup_geometry(%{"geometry" => geometry}), do: geometry

  defp lookup_geometry(%{"source" => source, "geometry_source_ids" => ids})
       when is_list(ids) and is_binary(source) do
    geoms =
      from g in Geometry,
        where: g.source == ^source and g.source_id in ^ids,
        select: g.geometry

    Repo.one(from g in subquery(geoms), select: st_union(g.geometry))
  end
end
