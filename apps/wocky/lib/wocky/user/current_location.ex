defmodule Wocky.User.CurrentLocation do
  @moduledoc false

  use Wocky.Repo.Schema

  alias Wocky.User

  @foreign_key_type :binary_id
  @primary_key false
  schema "user_current_location" do
    field :user_id, :binary_id, null: false, primary_key: true
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
    field :is_fetch, :boolean, default: false

    timestamps()

    belongs_to :user, User, define_field: false
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
          is_fetch: boolean | nil,
          created_at: DateTime.t() | nil,
          updated_at: DateTime.t() | nil
        }
end
