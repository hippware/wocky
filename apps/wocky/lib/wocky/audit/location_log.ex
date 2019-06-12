defmodule Wocky.Audit.LocationLog do
  @moduledoc false

  use Wocky.Repo.Schema

  alias Wocky.Account.User
  alias Wocky.Location.BotEvent

  @foreign_key_type :binary_id
  @primary_key {:id, :binary_id, autogenerate: true}
  schema "user_locations" do
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

    timestamps()

    belongs_to :user, User, define_field: false
    has_many :events, BotEvent, foreign_key: :location_id
  end

  @type t :: %__MODULE__{
          user_id: User.id() | nil,
          device: User.device(),
          lat: float,
          lon: float,
          accuracy: float,
          speed: float | nil,
          heading: float | nil,
          altitude: float | nil,
          altitude_accuracy: float | nil,
          captured_at: DateTime.t() | nil,
          uuid: String.t() | nil,
          is_moving: boolean | nil,
          odometer: float | nil,
          activity: String.t() | nil,
          activity_confidence: integer | nil,
          battery_level: float | nil,
          battery_charging: boolean | nil,
          created_at: DateTime.t() | nil,
          updated_at: DateTime.t() | nil
        }

  @insert_fields [
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
    :battery_charging
  ]

  @doc false
  def changeset(struct, params) do
    struct
    |> cast(params, @insert_fields)
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
end