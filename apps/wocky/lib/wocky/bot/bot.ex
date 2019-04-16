defmodule Wocky.Bot do
  @moduledoc "Schema and API for working with Bots."

  use Elixometer
  use Wocky.Repo.Schema

  import Ecto.Query

  alias Ecto.Association.NotLoaded
  alias Ecto.{Changeset, Queryable}
  alias Geocalc.Point
  alias Wocky.Block
  alias Wocky.Bot.{Invitation, Item, Subscription}
  alias Wocky.Events.GeofenceEvent
  alias Wocky.GeoUtils
  alias Wocky.Notifier
  alias Wocky.Repo
  alias Wocky.Repo.ID
  alias Wocky.Roster
  alias Wocky.User
  alias Wocky.User.GeoFence
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

  @type relationship ::
          :visible
          | :owned
          | :subscribed
          | :subscribed_not_owned
          | :invited
          | :visiting

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

  # ----------------------------------------------------------------------
  # Database interaction

  @spec get(Bot.id(), boolean) :: t | nil
  def get(id, include_pending \\ false)

  def get(id, include_pending) when is_binary(id) do
    id
    |> get_query(include_pending)
    |> Repo.one()
  end

  @doc false
  def get_query(id, include_pending \\ false) do
    Bot
    |> where(id: ^id)
    |> maybe_filter_pending(not include_pending)
  end

  defp maybe_filter_pending(queryable, false), do: queryable

  defp maybe_filter_pending(queryable, true),
    do: where(queryable, pending: false)

  @spec get_bot(id, User.t(), boolean) :: t | nil
  def get_bot(id, requestor, include_pending \\ false) do
    id
    |> get_bot_query(requestor, include_pending)
    |> Repo.one()
  end

  @spec get_bot_query(id, User.t(), boolean) :: Queryable.t()
  def get_bot_query(id, requestor, include_pending \\ false) do
    id
    |> get_query(include_pending)
    |> is_visible_query(requestor)
  end

  @spec is_visible_query(Queryable.t(), User.t()) :: Queryable.t()
  def is_visible_query(queryable, user) do
    queryable
    |> Block.object_visible_query(user)
    |> join(
      :left,
      [b, ...],
      invitation in Invitation,
      on: b.id == invitation.bot_id and invitation.invitee_id == ^user.id
    )
    |> join(
      :left,
      [b, ...],
      sub in Subscription,
      on: b.id == sub.bot_id and sub.user_id == ^user.id
    )
    |> where(
      [b, ..., invitation, sub],
      b.user_id == ^user.id or not is_nil(sub.user_id) or
        not is_nil(invitation.user_id)
    )
  end

  @spec get_owned_bot(id, User.t(), boolean) :: t | nil
  def get_owned_bot(id, %User{id: user_id}, include_pending \\ false) do
    Bot
    |> where(id: ^id, user_id: ^user_id)
    |> maybe_filter_pending(not include_pending)
    |> Repo.one()
  end

  @spec preallocate(User.t()) :: t | no_return
  def preallocate(user) do
    params = %{id: ID.new(), user_id: user.id, pending: true}

    %Bot{}
    |> cast(params, [:id, :user_id, :pending])
    |> foreign_key_constraint(:user_id)
    |> Repo.insert!()
  end

  @spec insert(map, User.t()) :: {:ok, t} | {:error, any}
  def insert(params, requestor) do
    with {:ok, t} <- do_update(%Bot{}, params, &Repo.insert/1) do
      update_counter("bot.created", 1)
      User.flag_bot_created(requestor)
      {:ok, t}
    end
  end

  @spec update(t, map) :: {:ok, t} | {:error, any}
  def update(bot, params) do
    do_update(bot, params, &Repo.update/1)
  end

  defp do_update(struct, params, op) do
    struct |> changeset(params) |> op.()
  end

  @spec bump_update_time(Bot.t()) :: :ok
  def bump_update_time(bot) do
    bot
    |> cast(%{updated_at: NaiveDateTime.utc_now()}, [:updated_at])
    |> Repo.update!()

    :ok
  end

  @spec delete(t) :: :ok
  def delete(bot) do
    Repo.delete(bot)
    update_counter("bot.deleted", 1)
    :ok
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

  # ----------------------------------------------------------------------
  # Bot relationships

  @spec subscription(t, User.t()) :: Subscription.state()
  def subscription(bot, user) do
    Subscription.state(user, bot)
  end

  @spec subscribe(t, User.t()) :: :ok | no_return
  def subscribe(bot, user) do
    with true <- Roster.self_or_friend?(user.id, bot.user_id) do
      Subscription.put(user, bot)
    else
      false -> {:error, :permission_denied}
    end
  end

  @spec unsubscribe(t, User.t()) :: :ok | {:error, any}
  def unsubscribe(bot, user) do
    GeoFence.exit_bot(user, bot, "unsubscribe")
    Subscription.delete(user, bot)
  end

  @spec visit(t, User.t(), boolean) :: :ok
  def visit(bot, user, notify) do
    Subscription.visit(user, bot)
    if notify, do: send_visit_notifications(user, bot, :enter)
    :ok
  end

  @spec depart(t, User.t(), boolean) :: :ok
  def depart(bot, user, notify) do
    Subscription.depart(user, bot)
    if notify, do: send_visit_notifications(user, bot, :exit)
    :ok
  end

  defp send_visit_notifications(visitor, bot, bot_event) do
    event = %GeofenceEvent{
      from: visitor,
      bot: bot,
      event: bot_event
    }

    bot
    |> notification_recipients(visitor)
    |> Enum.each(&do_send_visit_notification(&1, event))
  end

  defp do_send_visit_notification(subscriber, event),
    do: Notifier.notify(%GeofenceEvent{event | to: subscriber})

  @spec by_relationship_query(User.t(), relationship(), User.t() | nil) ::
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

  defp by_relationship_query(user, :invited) do
    join(
      Bot,
      :inner,
      [b],
      i in Invitation,
      on: b.id == i.bot_id and i.invitee_id == ^user.id
    )
  end

  defp by_relationship_query(user, :subscribed) do
    join(
      Bot,
      :inner,
      [b],
      s in Subscription,
      on: b.id == s.bot_id and s.user_id == ^user.id
    )
  end

  defp by_relationship_query(user, :subscribed_not_owned) do
    user
    |> by_relationship_query(:subscribed)
    |> where([b, ...], b.user_id != ^user.id)
  end

  defp by_relationship_query(user, :visiting) do
    user
    |> by_relationship_query(:subscribed)
    |> where([..., s], s.visitor)
    |> order_by([..., s], desc: s.visited_at)
  end

  @spec active_bots_query(User.t()) :: Ecto.Queryable.t()
  def active_bots_query(user) do
    user
    |> by_relationship_query(:subscribed)
    |> is_visible_query(user)
    |> join(
      :inner,
      [b],
      a in "bot_activity",
      on: b.id == a.bot_id
    )
    |> order_by([..., a], desc: a.visited_at)
  end

  @spec subscribers_query(t) :: [User.t()]
  def subscribers_query(bot) do
    Ecto.assoc(bot, :subscribers)
  end

  @spec subscriber_query(Bot.t(), User.id()) :: Queryable.t()
  def subscriber_query(bot, user_id) do
    bot
    |> Ecto.assoc(:subscribers)
    |> where([..., s], s.user_id == ^user_id)
  end

  @spec visitors_query(Bot.t()) :: Queryable.t()
  def visitors_query(bot) do
    bot
    |> Ecto.assoc(:subscribers)
    |> where([..., s], s.visitor)
    |> order_by([..., s], desc: s.visited_at)
  end

  @doc false
  @spec notification_recipients(Bot.t(), User.t()) :: [User.t()]
  def notification_recipients(bot, sender) do
    # TODO We might be able to reduce the round trips to the database
    # by combining the friends query and the subscriber query. That would
    # result in an ugly join, so I am doing the easy thing for now.
    bot = Repo.preload(bot, [:subscribers])
    subscribers = MapSet.new(bot.subscribers)

    friends =
      sender
      |> Roster.friends_query(sender)
      |> Repo.all()
      |> MapSet.new()
      |> MapSet.put(sender)

    subscribers
    |> MapSet.intersection(friends)
    |> MapSet.to_list()
  end

  @spec sub_setup_event(Bot.t()) :: Waiter.event()
  def sub_setup_event(bot), do: "bot_sub_setup-" <> bot.id

  # ----------------------------------------------------------------------
  # Location

  @spec lat(Bot.t()) :: float | nil
  def lat(%Bot{location: %Geo.Point{coordinates: {_, lat}}}), do: lat
  def lat(_), do: nil

  @spec lon(Bot.t()) :: float | nil
  def lon(%Bot{location: %Geo.Point{coordinates: {lon, _}}}), do: lon
  def lon(_), do: nil

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
end
