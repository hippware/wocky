defmodule Wocky.POI.Bot do
  @moduledoc "Schema and API for working with Bots."

  use Wocky.Repo.Schema

  alias Ecto.Changeset
  alias Wocky.Account.User
  alias Wocky.POI.Item
  alias Wocky.Relation.Invitation
  alias Wocky.Relation.Subscription

  @foreign_key_type :binary_id
  @primary_key {:id, :binary_id, autogenerate: false}
  schema "bots" do
    # Bot title
    field :title, :string, default: ""
    # True if this is a preallocated bot ID
    field :pending, :boolean
    # Bot shortname for URL representation
    field :shortname, :string
    # User-supplied description
    field :description, :string, default: ""
    # Bot graphical image TROS url
    field :image_url, :string
    # Bot type (freeform string from server's perspective)
    field :type, :string, default: ""
    # Bot icon (freeform string from server's perspective)
    field :icon, :string, default: ""
    # Free-form string field describing bot's location
    field :address, :string, default: ""
    # Opaque field containing adress related information
    field :address_data, :string, default: ""
    # Location
    field :location, Geo.PostGIS.Geometry
    # Radius of bot circle
    field :radius, :float, default: 100.0
    # Visibility of bot
    field :tags, {:array, :string}

    timestamps()

    belongs_to :user, User

    has_many :items, Item
    has_many :invitations, Invitation

    many_to_many(:subscribers, User, join_through: Subscription)
  end

  @type id :: String.t()

  @type t :: %__MODULE__{
          id: nil | id(),
          title: String.t(),
          pending: nil | boolean(),
          shortname: nil | String.t(),
          description: String.t(),
          image_url: nil | String.t(),
          type: String.t(),
          icon: nil | String.t(),
          address: String.t(),
          address_data: String.t(),
          location: nil | Geo.Point.t(),
          radius: nil | float(),
          tags: nil | [String.t()],
          user: Repo.not_loaded() | User.t(),
          items: Repo.not_loaded() | [Item.t()],
          invitations: Repo.not_loaded() | [Invitation.t()],
          subscribers: Repo.not_loaded() | [User.t()]
        }

  @change_fields [
    :id,
    :user_id,
    :title,
    :shortname,
    :description,
    :image_url,
    :type,
    :icon,
    :address,
    :address_data,
    :location,
    :radius,
    :tags
  ]
  @required_fields [:id, :user_id, :title, :location, :radius]

  @spec changeset(t(), map()) :: Changeset.t()
  def changeset(struct, params) do
    struct
    |> cast(params, @change_fields)
    |> validate_required(@required_fields)
    |> validate_number(:radius, greater_than: 0)
    |> validate_not_nil([:description])
    |> put_change(:pending, false)
    |> unique_constraint(:shortname)
    |> foreign_key_constraint(:user_id)
  end
end
