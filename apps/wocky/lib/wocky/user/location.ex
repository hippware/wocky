defmodule Wocky.User.Location do
  @moduledoc false

  use Wocky.Repo.Schema

  alias Wocky.GeoUtils
  alias Wocky.Repo
  alias Wocky.User
  alias Wocky.User.BotEvent

  @foreign_key_type :binary_id
  @primary_key {:id, :binary_id, autogenerate: true}
  schema "user_locations" do
    field :user_id, :binary_id, null: false
    field :resource, :string, null: false
    field :lat, :float, null: false
    field :lon, :float, null: false
    field :accuracy, :float

    timestamps()

    belongs_to :user, User, define_field: false
    has_many :events, BotEvent
  end

  @type location_tuple :: {float, float, float}
  @type t :: %Location{
          user_id: User.id(),
          resource: User.resource(),
          lat: float,
          lon: float,
          accuracy: float
        }

  @doc "Store a user location datapoint"
  @spec insert(User.t(), User.resource(), float, float, float) ::
          {:ok, t} | {:error, any}
  def insert(user, resource, lat, lon, accuracy) do
    {nlat, nlon} = GeoUtils.normalize_lat_lon(lat, lon)
    data = %{resource: resource, lat: nlat, lon: nlon, accuracy: accuracy}

    user
    |> Ecto.build_assoc(:locations)
    |> changeset(data)
    |> Repo.insert()
  end

  @spec new(User.t(), User.resource(), float(), float(), float()) :: t()
  def new(user, resource, lat, lon, accuracy) do
    {nlat, nlon} = GeoUtils.normalize_lat_lon(lat, lon)

    %Location{
      user_id: user.id,
      resource: resource,
      lat: nlat,
      lon: nlon,
      accuracy: accuracy
    }
  end

  @doc false
  def changeset(struct, params) do
    struct
    |> cast(params, [:resource, :lat, :lon, :accuracy])
    |> validate_required([:resource, :lat, :lon, :accuracy])
    |> validate_number(:accuracy, greater_than_or_equal_to: 0)
  end
end
