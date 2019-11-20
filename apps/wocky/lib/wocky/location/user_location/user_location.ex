defmodule Wocky.Location.UserLocation do
  @moduledoc false

  use Wocky.Repo.Schema

  alias Wocky.Account.User
  alias Wocky.GeoUtils

  embedded_schema do
    field :user_id, :binary_id, null: false
    field :device, :string, null: false
    field :lat, :float, null: false
    field :lon, :float, null: false
    field :accuracy, :float
    field :speed, :float
    field :heading, :float
    field :altitude, :float
    field :altitude_accuracy, :float
    field :captured_at, :utc_datetime_usec
    field :uuid, :string
    field :is_moving, :boolean
    field :odometer, :float
    field :activity, :string
    field :activity_confidence, :integer
    field :battery_level, :float
    field :battery_charging, :boolean
    field :extra_fields, :map
    field :created_at, :utc_datetime_usec
  end

  @type t :: %__MODULE__{
          id: String.t(),
          user_id: User.id() | nil,
          device: User.device(),
          lat: float(),
          lon: float(),
          accuracy: float(),
          speed: float() | nil,
          heading: float() | nil,
          altitude: float() | nil,
          altitude_accuracy: float() | nil,
          captured_at: DateTime.t() | nil,
          uuid: String.t() | nil,
          is_moving: boolean() | nil,
          odometer: float() | nil,
          activity: String.t() | nil,
          activity_confidence: integer() | nil,
          battery_level: float() | nil,
          battery_charging: boolean() | nil,
          extra_fields: map(),
          created_at: DateTime.t() | nil
        }

  @fields [
    :device,
    :lat,
    :lon,
    :accuracy,
    :speed,
    :heading,
    :altitude,
    :altitude_accuracy,
    :captured_at,
    :uuid,
    :is_moving,
    :odometer,
    :activity,
    :activity_confidence,
    :battery_level,
    :battery_charging,
    :created_at
  ]

  @doc false
  @spec changeset(struct(), map()) :: Changeset.t()
  def changeset(struct, params) do
    struct
    |> cast(params, @fields)
    |> validate_required([:device, :lat, :lon, :accuracy, :captured_at])
    |> validate_number(:accuracy, greater_than_or_equal_to: 0)
    |> validate_number(
      :lat,
      greater_than_or_equal_to: -90,
      less_than_or_equal_to: 90
    )
    |> validate_number(
      :lon,
      greater_than_or_equal_to: -180,
      less_than_or_equal_to: 180
    )
  end

  @spec new(map()) :: t()
  def new(fields) do
    {nlat, nlon} = GeoUtils.normalize_lat_lon(fields.lat, fields.lon)

    nfields =
      fields
      |> Map.put(:lat, nlat)
      |> Map.put(:lon, nlon)
      |> Map.put(:captured_at, normalize_captured_at(fields))
      |> Map.put(:created_at, DateTime.utc_now())

    %__MODULE__{}
    |> changeset(nfields)
    |> apply_changes()
  end

  @spec validate(t()) :: :ok | {:error, Changeset.t()}
  def validate(location) do
    fields = Map.from_struct(location)
    changeset = changeset(%__MODULE__{}, fields)

    if changeset.valid? do
      :ok
    else
      {:error, changeset}
    end
  end

  defp normalize_captured_at(%{captured_at: time})
       when is_binary(time) do
    {:ok, dt, 0} = DateTime.from_iso8601(time)
    dt
  end

  defp normalize_captured_at(%{captured_at: %DateTime{} = dt}),
    do: dt

  defp normalize_captured_at(_), do: DateTime.utc_now()
end
