defmodule Wocky.Bot do
  @moduledoc ""

  use Wocky.Repo.Model
  use Wocky.JID

  alias Wocky.Bot.Item
  alias Wocky.Bot.Share
  alias Wocky.Bot.Subscription
  alias Wocky.Bot.TempSubscription
  alias Wocky.GeoUtils
  alias Wocky.Index
  alias Wocky.Repo.ID
  alias Wocky.User
  alias __MODULE__, as: Bot

  require Record

  @foreign_key_type :binary_id
  @primary_key {:id, :binary_id, autogenerate: false}
  schema "bots" do
    field :server,           :string  # Bot server
    field :title,            :string  # Bot title
    field :pending,          :boolean # True if this is a preallocated bot ID
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
    field :public,           :boolean # Visibility of bot
    field :alerts,           :boolean # Whether alerts are enabled
    field :follow_me,        :boolean # Does bot follow owner
    field :follow_me_expiry, :integer # When follow me expires

    timestamps()

    belongs_to :user, User

    has_many :items, Item

    many_to_many :shares, User, join_through: Share
    many_to_many :subscribers, User, join_through: Subscription
    many_to_many :temp_subscribers, User, join_through: TempSubscription
  end

  @type id :: binary
  @type t :: %Bot{}

  @bot_prefix "bot/"
  @change_fields [:id, :server, :user_id, :title, :shortname, :description,
                  :image, :type, :address, :lat, :lon, :radius, :public,
                  :alerts, :follow_me, :follow_me_expiry]
  @required_fields [:id, :server, :user_id, :title, :lat, :lon, :radius]

  @spec make_node(t) :: binary
  def make_node(bot) do
    @bot_prefix <> bot.id
  end

  @spec to_jid(t) :: JID.t
  def to_jid(bot) do
    JID.make("", bot.server, make_node(bot))
  end

  @spec get_id_from_jid(JID.t) :: id | nil
  def get_id_from_jid(jid(lresource: @bot_prefix <> id)), do: id
  def get_id_from_jid({_, _, @bot_prefix <> id}), do: id
  def get_id_from_jid(_), do: nil

  @spec get_id_from_node(binary) :: id | nil
  def get_id_from_node(@bot_prefix <> id), do: id
  def get_id_from_node(_), do: nil

  @spec new :: t
  def new, do: %Bot{}

  @spec get(id) :: t | nil
  def get(id) do
    Repo.get(Bot, id)
  end

  @spec all :: [t]
  def all do
    Repo.all(Bot)
  end

  @spec preallocate(User.id, User.server) :: t | no_return
  def preallocate(user_id, server) do
    params = %{id: ID.new, server: server, user_id: user_id, pending: true}

    %Bot{}
    |> cast(params, [:id, :server, :user_id, :pending])
    |> foreign_key_constraint(:user_id)
    |> Repo.insert!
  end

  @spec pending?(id) :: boolean
  def pending?(bot_id) do
    case get(bot_id) do
      nil -> false
      bot -> bot.pending
    end
  end

  @spec changeset(t, map) :: Changeset.t
  def changeset(struct, params \\ %{}) do
    struct
    |> cast(params, @change_fields)
    |> validate_required(@required_fields)
    |> put_change(:pending, false)
    |> unique_constraint(:shortname)
    |> foreign_key_constraint(:user_id)
  end

  @spec insert(map) :: {:ok, t} | {:error, any}
  def insert(params) do
    %Bot{}
    |> changeset(params)
    |> Repo.insert
  end

  @spec update(t, map) :: {:ok, t} | {:error, any}
  def update(bot, params) do
    bot
    |> changeset(params)
    |> Repo.update
  end

  @spec set_location(t, float, float, float) :: :ok
  def set_location(%Bot{id: id} = bot, lat, lon, _accuracy) do
    bot
    |> cast(%{lat: lat, lon: lon}, [:lat, :lon])
    |> update_change(:lat, &GeoUtils.normalize_latitude/1)
    |> update_change(:lon, &GeoUtils.normalize_longitude/1)
    |> Repo.update!

    Index.bot_updated(id, %{lat: lat, lon: lon})
  end

  @spec items(t) :: [Item.t]
  def items(bot) do
    Item.get(bot)
  end

  @spec image_items(t) :: [Item.t]
  def image_items(bot) do
    Item.get_images(bot)
  end

  @spec image_items_count(t) :: pos_integer
  def image_items_count(bot) do
    Item.get_image_count(bot)
  end

  @spec publish_item(t, binary, binary, boolean) :: {:ok, Item.t}
  def publish_item(bot, id, stanza, image?) do
    :ok = Item.put(bot, id, stanza, image?)
    {:ok, Item.get(bot, id)}
  end

  @spec delete_item(Bot.t, binary) :: :ok
  def delete_item(bot, id) do
    Item.delete(bot, id)
  end

  @spec owner(t) :: User.t
  def owner(bot) do
    bot = Repo.preload(bot, :user)
    bot.user
  end

  @spec subscribers(t) :: [User.t]
  def subscribers(bot) do
    bot = Repo.preload(bot, [:subscribers, :temp_subscribers])

    [bot.subscribers, bot.temp_subscribers]
    |> List.flatten
    |> Enum.sort_by(&(&1.id))
    |> Enum.uniq_by(&(&1.id))
  end

  @spec subscriber_count(Bot.t) :: pos_integer
  def subscriber_count(bot) do
    subscribers =
      bot
      |> assoc(:subscribers)
      |> select([s], count(s.id))
      |> Repo.one

    temp_subscribers =
      bot
      |> assoc(:temp_subscribers)
      |> select([s], count(s.id))
      |> Repo.one

    subscribers + temp_subscribers + 1
  end

  @spec public?(t) :: boolean
  def public?(%Bot{public: is_public}), do: is_public

  @spec share(t, User.t, User.t) :: :ok
  def share(bot, to, from) do
    Share.put(to, bot, from)
  end

  @spec shared_to?(t, User.t) :: boolean
  def shared_to?(bot, user) do
    Share.exists?(user, bot)
  end

  @spec follow_me(t, pos_integer) :: {:ok, t} | {:error, any}
  def follow_me(bot, expiry) do
    bot
    |> changeset(%{follow_me: true, follow_me_expiry: expiry})
    |> Repo.update
  end

  @spec unfollow_me(t) :: {:ok, t} | {:error, any}
  def unfollow_me(bot) do
    bot
    |> changeset(%{follow_me: false, follow_me_expiry: nil})
    |> Repo.update
  end

  @spec delete(t) :: :ok
  def delete(bot) do
    Repo.delete(bot)
    :ok
  end
end
