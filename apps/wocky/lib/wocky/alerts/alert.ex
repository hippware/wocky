defmodule Wocky.Alerts.Alert do
  @moduledoc "Ecto schema for an alert."

  use Wocky.Repo.Schema

  @foreign_key_type :binary_id
  @primary_key {:id, :binary_id, autogenerate: true}
  schema "safety_alerts" do
    field :source, :string, null: false
    field :source_id, :string, null: false
    field :expires_at, :utc_datetime_usec
    field :title, :string, null: false
    field :summary, :string, null: false
    field :link, :string
    field :geometry, Geo.PostGIS.Geometry, null: false
    field :data, :map, null: false, default: %{}
    field :imported, :boolean, null: false, default: true

    timestamps()
  end

  @type t :: %__MODULE__{}

  @fields [
    :source,
    :source_id,
    :expires_at,
    :title,
    :summary,
    :link,
    :geometry,
    :data,
    :imported
  ]

  @spec changeset(map()) :: Changeset.t()
  def changeset(params) do
    %__MODULE__{}
    |> cast(params, @fields)
    |> validate_required([:source, :source_id, :title, :summary, :geometry])
    |> put_change(:imported, true)
  end
end
