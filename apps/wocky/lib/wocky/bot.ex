defmodule Wocky.Bot do
  @moduledoc ""

  use Wocky.Repo.Model
  use Wocky.JID

  alias Wocky.Index
  alias __MODULE__, as: Bot

  require Record

  @foreign_key_type :binary_id
  @primary_key {:id, :binary_id, autogenerate: false}
  schema "bots" do
    field :server,           :string  # Bot server
    field :title,            :string  # Bot title
    field :shortname,        :string  # Bot shortname for URL representation
    field :description,      :string  # User-supplied description
    field :image,            :string  # Bot graphical image
    field :type,             :string  # Bot type (freeform string from
                                      # server's perspective)
    field :address,          :string  # Free-form string field describing bot's
                                      # location
    field :lat,              :float   # Latitude
    field :lon,              :float   # Longitude
    field :radius,           :integer # Radius of bot circle
    field :visibility,       :integer # Visibility of bot
    field :alerts,           :boolean # Whether alerts are enabled
    field :follow_me,        :boolean # Does bot follow owner
    field :follow_me_expiry, :integer # When follow me expires

    timestamps()

    belongs_to :user, User

    many_to_many :subscribers, User, join_through: "bot_subscribers"
  end

  @type id           :: binary

  @type t :: %Bot{
    id:               id,
    server:           binary,
    title:            nil | binary,
    shortname:        nil | binary,
    description:      nil | binary,
    image:            nil | binary,
    type:             nil | binary,
    address:          nil | binary,
    lat:              nil | float,
    lon:              nil | float,
    radius:           nil | integer,
    visibility:       nil | integer,
    alerts:           nil | boolean,
    follow_me:        nil | boolean,
    follow_me_expiry: nil | integer
  }

  @spec to_jid(t) :: JID.t
  def to_jid(bot) do
    JID.make("", bot.server, bot.id)
  end

  @spec to_jid_string(t) :: binary
  def to_jid_string(bot) do
    bot |> to_jid |> JID.to_binary
  end

  @spec get_id_from_jid(JID.t) :: id
  def get_id_from_jid(jid(lresource: "bot/" <> id)), do: id
  def get_id_from_jid({_, _, "bot/" <> id}), do: id
  def get_id_from_jid(_), do: ""

  @spec get(id) :: t | nil
  def get(id) do
    Repo.get(Bot, id)
  end

  @spec set_location(t, float, float, float) :: :ok
  def set_location(%Bot{id: id} = bot, lat, lon, _accuracy) do
    bot
    |> location_changeset(%{lat: lat, lon: lon})
    |> Repo.update!

    Index.bot_updated(id, %{lat: lat, lon: lon})
  end

  defp location_changeset(struct, params) do
    struct
    |> cast(params, [:lat, :lon])
    |> validate_required([:lat, :lon])
  end
end
