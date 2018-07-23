defmodule Wocky.Bot do
  @moduledoc "Schema and API for working with Bots."

  use Elixometer
  use Wocky.JID
  use Wocky.Repo.Schema
  use Wocky.Repo.Changeset
  use Wocky.RSMHelper

  import Ecto.Query

  alias Ecto.Association.NotLoaded
  alias Ecto.{Changeset, Queryable, UUID}
  alias Geocalc.Point
  alias Wocky.Block
  alias Wocky.Bot.Item
  alias Wocky.Bot.Share
  alias Wocky.Bot.Subscription
  alias Wocky.GeoUtils
  alias Wocky.HomeStream
  alias Wocky.Index
  alias Wocky.Push
  alias Wocky.Push.Events.BotPerimeterEvent
  alias Wocky.Repo
  alias Wocky.Repo.ID
  alias Wocky.User
  alias Wocky.Waiter

  require Logger
  require Record

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
    # Bot graphical image
    field :image, :string
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
    field :public, :boolean
    field :tags, {:array, :string}
    field :geofence, :boolean, default: false

    timestamps()

    belongs_to :user, User

    has_many :items, Item

    many_to_many(:shares, User, join_through: Share)
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
          image: nil | binary,
          type: binary,
          icon: nil | binary,
          address: binary,
          address_data: binary,
          location: nil | Geo.Point.t(),
          radius: nil | float,
          public: nil | boolean,
          tags: nil | [binary],
          geofence: boolean,
          user: not_loaded | User.t(),
          items: not_loaded | [Item.t()],
          shares: not_loaded | [User.t()],
          subscribers: not_loaded | [User.t()]
        }

  @bot_prefix "bot/"
  @change_fields [
    :id,
    :user_id,
    :title,
    :shortname,
    :description,
    :image,
    :type,
    :icon,
    :address,
    :address_data,
    :location,
    :radius,
    :public,
    :tags,
    :geofence
  ]
  @required_fields [:id, :user_id, :title, :location, :radius]

  # ----------------------------------------------------------------------
  # Helpers

  @spec make_node(t) :: binary
  def make_node(bot) do
    @bot_prefix <> bot.id
  end

  @spec to_jid(t) :: JID.t()
  def to_jid(bot) do
    JID.make("", Wocky.host(), make_node(bot))
  end

  @spec get_id_from_jid(JID.t()) :: id | nil
  def get_id_from_jid(jid(lresource: @bot_prefix <> id)) do
    case ID.valid?(id) do
      true -> id
      false -> nil
    end
  end

  def get_id_from_jid(_), do: nil

  @spec get_id_from_node(binary) :: id | nil
  def get_id_from_node(@bot_prefix <> id), do: id
  def get_id_from_node(_), do: nil

  # public? takes a bare map rather than requiring a %Bot struct because it's
  # used by the bot geosearch. The geosearch retrives partially complete bots
  # from algolia and stores them in a map. Rather than hitting the DB and
  # fililng out the Bot struct, we just allow those maps to be used directly
  # here.
  @spec public?(map) :: boolean
  def public?(%{public: is_public}), do: is_public

  # ----------------------------------------------------------------------
  # Database interaction

  @spec get(Bot.id() | JID.t(), boolean) :: t | nil
  def get(id, include_pending \\ false)

  def get(id, include_pending) when is_binary(id) do
    id
    |> get_query(include_pending)
    |> Repo.one()
  end

  def get(jid, include_pending) when Record.is_record(jid, :jid) do
    case get_id_from_jid(jid) do
      nil -> nil
      id -> get(id, include_pending)
    end
  end

  @doc false
  def get_query(id, include_pending \\ false) do
    Bot
    |> where(id: ^id)
    |> maybe_filter_pending(not include_pending)
  end

  @spec get_bot(id, User.t() | nil, boolean) :: t | nil
  def get_bot(id, requestor \\ nil, include_pending \\ false) do
    id
    |> get_bot_query(requestor, include_pending)
    |> Repo.one()
  end

  @spec get_bot_query(id, User.t() | nil, boolean) :: Queryable.t()
  def get_bot_query(id, requestor \\ nil, include_pending \\ false) do
    id
    |> get_query(include_pending)
    |> is_visible_query(requestor)
  end

  @spec get_owned_bot(id, User.t(), boolean) :: t | nil
  def get_owned_bot(id, %User{id: user_id}, include_pending \\ false) do
    Bot
    |> where(id: ^id, user_id: ^user_id)
    |> maybe_filter_pending(not include_pending)
    |> Repo.one()
  end

  @spec preallocate(User.id()) :: t | no_return
  def preallocate(user_id) do
    params = %{id: ID.new(), user_id: user_id, pending: true}

    %Bot{}
    |> cast(params, [:id, :user_id, :pending])
    |> foreign_key_constraint(:user_id)
    |> Repo.insert!()
  end

  @spec changeset(t, map) :: Changeset.t()
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

  @spec insert(map) :: {:ok, t} | {:error, any}
  def insert(params) do
    with {:ok, t} <- do_update(%Bot{}, params, &Repo.insert/1) do
      update_counter("bot.created", 1)
      {:ok, t}
    end
  end

  @spec update(t, map) :: {:ok, t} | {:error, any}
  def update(bot, params) do
    do_update(bot, params, &Repo.update/1)
  end

  defp do_update(struct, params, op) do
    case struct |> changeset(params) |> op.() do
      {:ok, bot} = result ->
        Index.update(:bot, bot.id, bot)
        result

      {:error, _} = error ->
        error
    end
  end

  @spec delete(t) :: :ok
  def delete(bot) do
    Repo.delete(bot)
    Index.remove(:bot, bot.id)
    update_counter("bot.deleted", 1)
    :ok
  end

  @spec owner(t) :: User.t()
  def owner(bot) do
    bot = Repo.preload(bot, :user)
    bot.user
  end

  @spec subscription(t, User.t()) :: Subscription.state()
  def subscription(bot, user) do
    Subscription.state(user, bot)
  end

  @spec subscribe(t, User.t(), boolean()) :: :ok | no_return
  def subscribe(bot, user, guest \\ false) do
    if guest == false && subscription(bot, user) == :visitor do
      send_visit_notifications(user, bot, :exit)
    end

    Subscription.put(user, bot, guest)
  end

  @spec unsubscribe(t, User.t()) :: :ok | {:error, any}
  def unsubscribe(bot, user) do
    if subscription(bot, user) == :visitor do
      send_visit_notifications(user, bot, :exit)
    end

    Subscription.delete(user, bot)
  end

  @spec visit(t, User.t(), boolean) :: :ok
  def visit(bot, user, notify \\ true) do
    Subscription.visit(user, bot)
    if notify, do: send_visit_notifications(user, bot, :enter)
    :ok
  end

  @spec depart(t, User.t(), boolean) :: :ok
  def depart(bot, user, notify \\ true) do
    Subscription.depart(user, bot)
    if notify, do: send_visit_notifications(user, bot, :exit)
    :ok
  end

  defp send_visit_notifications(visitor, bot, event) do
    if visit_notifications_enabled?() do
      bot
      |> guests_query()
      |> User.filter_hidden()
      |> Repo.all()
      |> Enum.each(&send_visit_notification(&1, visitor, bot, event))
    end
  end

  defp visit_notifications_enabled? do
    Application.fetch_env!(:wocky, :enable_bot_event_notifications)
  end

  defp send_visit_notification(subscriber, visitor, bot, event) do
    event = %BotPerimeterEvent{
      user: visitor,
      bot: bot,
      event: event
    }

    Push.notify_all(subscriber.id, event)
  end

  @spec clear_guests(t) :: :ok
  def clear_guests(bot) do
    Subscription.clear_guests(bot)
  end

  @spec by_relationship_query(User.t(), atom(), User.t() | nil) ::
          Ecto.Queryable.t()
  def by_relationship_query(user, rel, user) do
    by_relationship_query(user, rel)
  end

  def by_relationship_query(user, rel, requestor) do
    user
    |> by_relationship_query(rel)
    |> is_visible_query(requestor)
  end

  defp by_relationship_query(user, :visible) do
    is_visible_query(Bot, user)
  end

  defp by_relationship_query(user, :owned) do
    User.owned_bots_query(user)
  end

  defp by_relationship_query(user, :shared) do
    join(
      Bot,
      :inner,
      [b],
      s in Share,
      b.id == s.bot_id and s.user_id == ^user.id
    )
  end

  defp by_relationship_query(user, :subscribed) do
    join(
      Bot,
      :inner,
      [b],
      s in Subscription,
      b.id == s.bot_id and s.user_id == ^user.id
    )
  end

  defp by_relationship_query(user, :subscribed_not_owned) do
    user
    |> by_relationship_query(:subscribed)
    |> where([b, ...], b.user_id != ^user.id)
  end

  defp by_relationship_query(user, :guest) do
    user
    |> by_relationship_query(:subscribed)
    |> where([..., s], s.guest)
  end

  defp by_relationship_query(user, :visitor) do
    user
    |> by_relationship_query(:subscribed)
    |> where([..., s], s.visitor)
    |> order_by([..., s], desc: s.visited_at)
  end

  @spec active_bots_query(User.t()) :: Ecto.Queryable.t()
  def active_bots_query(user) do
    user
    |> by_relationship_query(:guest)
    |> is_visible_query(user)
    |> join(
      :inner,
      [b],
      a in "bot_activity",
      b.id == a.bot_id
    )
    |> order_by([..., a], desc: a.visited_at)
  end

  @spec discover_query(User.t(), DateTime.t() | nil) :: Queryable.t()
  def discover_query(user, since) do
    {:ok, user_id_bin} = UUID.dump(user.id)

    Bot
    |> join(:left, [b], u in User, b.user_id == u.id)
    |> where([b, u], b.public)
    |> where([b, u], fragment("is_follower(?, ?)", ^user_id_bin, u.id))
    |> maybe_add_since(since)
    |> order_by([b], asc: b.created_at)
  end

  def related_geofence_bots_query(user) do
    Bot
    |> join(
      :left,
      [b],
      s in Subscription,
      b.id == s.bot_id and s.user_id == ^user.id
    )
    |> where([b, s], s.guest or (b.geofence and b.user_id == ^user.id))
  end

  @spec subscribers_query(t, boolean()) :: [User.t()]
  def subscribers_query(bot, include_owner \\ true) do
    q = Ecto.assoc(bot, :subscribers)

    case include_owner do
      false -> where(q, [u], u.id != ^bot.user_id)
      true -> q
    end
  end

  @spec subscriber_query(Bot.t(), User.id()) :: Queryable.t()
  def subscriber_query(bot, user_id) do
    bot
    |> Ecto.assoc(:subscribers)
    |> where([..., s], s.user_id == ^user_id)
  end

  @spec guests_query(Bot.t()) :: Queryable.t()
  def guests_query(bot) do
    bot
    |> Ecto.assoc(:subscribers)
    |> where([..., s], s.guest)
  end

  @spec visitors_query(Bot.t()) :: Queryable.t()
  def visitors_query(bot) do
    bot
    |> Ecto.assoc(:subscribers)
    |> where([..., s], s.visitor)
    |> order_by([..., s], desc: s.visited_at)
  end

  @spec subscribers(t) :: [User.t()]
  def subscribers(bot) do
    Repo.preload(bot, [:subscribers]).subscribers
  end

  defp tidy_subscribers(subscribers) do
    subscribers
    |> Enum.sort_by(& &1.id)
    |> Enum.uniq_by(& &1.id)
  end

  @spec notification_recipients(Bot.t(), User.t()) :: [JID.t()]
  def notification_recipients(bot, sender) do
    bot = Repo.preload(bot, [:user])

    bot
    |> subscribers()
    |> Enum.concat([bot.user])
    |> tidy_subscribers()
    |> Enum.filter(&(&1.id != sender.id))
    |> Enum.map(&User.to_jid(&1))
  end

  @doc "Count of all subscribers"
  @spec subscriber_count(Bot.t()) :: pos_integer
  def subscriber_count(bot) do
    bot
    |> Ecto.assoc(:subscribers)
    |> select([s], count(s.id))
    |> Repo.one()
  end

  @doc "Returns the bot's distance from the specified location in meters."
  @spec distance_from(Bot.t(), Point.t()) :: float
  def distance_from(bot, loc) do
    Geocalc.distance_between(%{lat: lat(bot), lon: lon(bot)}, loc)
  end

  @doc "Returns true if the location is within the bot's radius."
  @spec contains?(Bot.t(), Point.t()) :: boolean
  def contains?(bot, loc) do
    distance_from(bot, loc) <= bot.radius
  end

  @spec is_visible_query(Queryable.t(), User.t() | nil) :: Queryable.t()
  def is_visible_query(queryable, nil) do
    queryable |> where([b, ...], b.public)
  end

  def is_visible_query(queryable, user) do
    queryable
    |> Block.object_visible_query(user.id)
    |> join(
      :left,
      [b, ...],
      s in Share,
      b.id == s.bot_id and s.user_id == ^user.id
    )
    |> where(
      [b, ..., s],
      b.user_id == ^user.id or b.public or not is_nil(s.user_id)
    )
  end

  def filter_by_location(query, point_a, point_b) do
    query
    |> where(
      fragment(
        "location && ST_MakeEnvelope(?, ?, ?, ?, 4326)::geography",
        ^GeoUtils.get_lon(point_a),
        ^GeoUtils.get_lat(point_a),
        ^GeoUtils.get_lon(point_b),
        ^GeoUtils.get_lat(point_b)
      )
    )
  end

  @spec bump_update_time(Bot.t()) :: :ok
  def bump_update_time(bot) do
    bot
    |> cast(%{updated_at: NaiveDateTime.utc_now()}, [:updated_at])
    |> Repo.update!()

    :ok
  end

  @spec lat(Bot.t()) :: float | nil
  def lat(%Bot{location: %Geo.Point{coordinates: {_, lat}}}), do: lat
  def lat(_), do: nil

  @spec lon(Bot.t()) :: float | nil
  def lon(%Bot{location: %Geo.Point{coordinates: {lon, _}}}), do: lon
  def lon(_), do: nil

  @spec fix_from_json(Bot.t()) :: Bot.t()
  def fix_from_json(%Bot{location: nil} = bot), do: bot

  def fix_from_json(%Bot{location: location} = bot) do
    new_loc =
      location
      |> GeoUtils.get_lat_lon()
      |> Tuple.to_list()
      |> Enum.map(&ensure_float/1)
      |> List.to_tuple()
      |> GeoUtils.point()

    Map.put(bot, :location, new_loc)
  end

  defp ensure_float(i) when is_integer(i), do: i / 1
  defp ensure_float(f) when is_float(f), do: f

  defp maybe_filter_pending(queryable, false) do
    queryable
  end

  defp maybe_filter_pending(queryable, true) do
    queryable
    |> where(pending: false)
  end

  def maybe_update_hs_items(old, new) do
    if should_update_hs(old, new) do
      HomeStream.update_ref_bot(new)
    end
  end

  defp should_update_hs(bot1, bot2) do
    [:title, :image, :address, :location, :public, :geofence]
    |> Enum.any?(fn f -> Map.get(bot1, f) != Map.get(bot2, f) end)
  end

  @spec sub_setup_event(Bot.t()) :: Waiter.event()
  def sub_setup_event(bot), do: "bot_sub_setup-" <> bot.id

  defp maybe_add_since(queryable, nil), do: queryable

  defp maybe_add_since(queryable, since) do
    queryable |> where([b, ...], b.created_at > ^since)
  end
end
