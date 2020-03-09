defmodule Wocky.Alerts.Geometry do
  @moduledoc "Ecto schema for alert geometry."

  use Wocky.Repo.Schema

  schema "safety_alerts_geometries" do
    field :source, :string, null: false
    field :source_id, :string, null: false
    field :geometry, Geo.PostGIS.Geometry, null: false
    field :data, :map, null: false, default: %{}

    timestamps()
  end

  @type t :: %__MODULE__{}

  @spec changeset(map()) :: Changeset.t()
  def changeset(params) do
    %__MODULE__{}
    |> cast(params, [:source, :source_id, :geometry, :data])
    |> validate_required([:source, :source_id, :geometry])
  end
end
