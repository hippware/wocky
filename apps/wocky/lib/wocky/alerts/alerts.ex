defmodule Wocky.Alerts do
  @moduledoc "Context for safety alerts."

  use Wocky.Context

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
    params
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
end
