defmodule Wocky.Bots.Bot do
  @moduledoc "Schema and API for working with Bots."

  use Wocky.Repo.Schema

  alias Ecto.Association.NotLoaded
  alias Ecto.Changeset
  alias Wocky.Account.User
  alias Wocky.Bots.Item
  alias Wocky.Relations.Invitation
  alias Wocky.Relations.Subscription

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

  @type id :: binary
  @type not_loaded :: %NotLoaded{}

  @type t :: %Bot{
          id: nil | id,
          title: binary,
          pending: nil | boolean,
          shortname: nil | binary,
          description: binary,
          image_url: nil | binary,
          type: binary,
          icon: nil | binary,
          address: binary,
          address_data: binary,
          location: nil | Geo.Point.t(),
          radius: nil | float,
          tags: nil | [binary],
          user: not_loaded | User.t(),
          items: not_loaded | [Item.t()],
          invitations: not_loaded | [Invitation.t()],
          subscribers: not_loaded | [User.t()]
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
