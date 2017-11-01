defmodule Wocky.Bot do
  @moduledoc ""

  use Wocky.JID
  use Wocky.Repo.Model
  use Wocky.RSMHelper

  alias Ecto.Changeset
  alias Ecto.Queryable
  alias Geocalc.Point
  alias Wocky.Blocking
  alias Wocky.Bot.Item
  alias Wocky.Bot.Share
  alias Wocky.Bot.Subscription
  alias Wocky.HomeStreamItem
  alias Wocky.Index
  alias Wocky.Repo.ID
  alias Wocky.User
  alias __MODULE__, as: Bot

  require Logger
  require Record

  @foreign_key_type :binary_id
  @primary_key {:id, :binary_id, autogenerate: false}
  schema "bots" do
    field :server,           :string  # Bot server
    field :title,            :string, default: ""  # Bot title
    field :pending,          :boolean # True if this is a preallocated bot ID
    field :shortname,        :string  # Bot shortname for URL representation
    field :description,      :string, default: ""  # User-supplied description
    field :image,            :string  # Bot graphical image
    field :type,             :string, default: ""
    # Bot type (freeform string from server's perspective)
    field :address,          :string, default: ""
    # Free-form string field describing bot's location
    field :address_data,     :string, default: ""
    # Opaque field containing adress related information
    field :location,         Geo.Point # Location
    field :radius,           :float   # Radius of bot circle
    field :public,           :boolean # Visibility of bot
    field :alerts,           :boolean # Whether alerts are enabled
    field :follow_me,        :boolean # Does bot follow owner
    field :follow_me_expiry, :utc_datetime # When follow me expires
    field :tags,             {:array, :string}

    timestamps()

    belongs_to :user, User

    has_many :items, Item

    many_to_many :shares, User, join_through: Share
    many_to_many :subscribers, User, join_through: Subscription
  end

  @type id :: binary
  @type t :: %Bot{}

  @bot_prefix "bot/"
  @change_fields [:id, :server, :user_id, :title, :shortname, :description,
                  :image, :type, :address, :address_data, :location, :radius,
                  :public, :alerts, :follow_me, :follow_me_expiry, :tags]
  @required_fields [:id, :server, :user_id, :title, :location, :radius]

  #----------------------------------------------------------------------
  # Helpers

  @spec make_node(t) :: binary
  def make_node(bot) do
    @bot_prefix <> bot.id
  end

  @spec to_jid(t) :: JID.t
  def to_jid(bot) do
    JID.make("", bot.server, make_node(bot))
  end

  @spec get_id_from_jid(JID.t) :: id | nil
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

  #----------------------------------------------------------------------
  # Database interaction

  @spec get(Bot.id | JID.t, boolean) :: t | nil
  def get(id, include_pending \\ false)
  def get(id, include_pending) when is_binary(id) do
    Bot
    |> where(id: ^id)
    |> maybe_filter_pending(not include_pending)
    |> Repo.one
  end
  def get(jid, include_pending) when Record.is_record(jid, :jid) do
    case get_id_from_jid(jid) do
      nil -> nil
      id -> get(id, include_pending)
    end
  end

  @spec preallocate(User.id, User.server) :: t | no_return
  def preallocate(user_id, server) do
    params = %{id: ID.new, server: server, user_id: user_id, pending: true}

    %Bot{}
    |> cast(params, [:id, :server, :user_id, :pending])
    |> foreign_key_constraint(:user_id)
    |> Repo.insert!
  end

  @spec changeset(t, map) :: Changeset.t
  def changeset(struct, params) do
    struct
    |> cast(params, @change_fields)
    |> validate_required(@required_fields)
    |> validate_number(:radius, greater_than: 0)
    |> put_change(:pending, false)
    |> unique_constraint(:shortname)
    |> foreign_key_constraint(:user_id)
  end

  @spec insert(map) :: {:ok, t} | {:error, any}
  def insert(params), do: do_update(%Bot{}, params, &Repo.insert/1)

  @spec update(t, map) :: {:ok, t} | {:error, any}
  def update(bot, params) do
    maybe_tidy_home_streams(bot, params)
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
    HomeStreamItem.delete_by_bot_ref(bot)
    Repo.delete(bot)
    Index.remove(:bot, bot.id)
    :ok
  end

  @spec owner(t) :: User.t
  def owner(bot) do
    bot = Repo.preload(bot, :user)
    bot.user
  end

  @spec subscribers(t) :: [User.t]
  def subscribers(bot) do
    Repo.preload(bot, [:subscribers]).subscribers
  end

  defp tidy_subscribers(subscribers) do
    subscribers
    |> Enum.sort_by(&(&1.id))
    |> Enum.uniq_by(&(&1.id))
  end

  @spec notification_recipients(Bot.t, User.t) :: [JID.t]
  def notification_recipients(bot, sender) do
    bot = Repo.preload(bot, [:user])

    bot
    |> subscribers()
    |> Enum.concat([bot.user])
    |> tidy_subscribers()
    |> Enum.filter(&(&1.id != sender.id))
    |> Enum.map(&User.to_jid(&1))
  end

  @doc "Count of all subscribers plus the owner"
  @spec subscriber_count(Bot.t) :: pos_integer
  def subscriber_count(bot) do
    bot
    |> assoc(:subscribers)
    |> select([s], count(s.id))
    |> Repo.one
    |> Kernel.+(1)
  end

  @doc "Returns the bot's distance from the specified location in meters."
  @spec distance_from(Bot.t, Point.t) :: non_neg_integer
  def distance_from(bot, loc) do
    Geocalc.distance_between(%{lat: lat(bot), lon: lon(bot)}, loc)
  end

  @doc "Returns true if the location is within the bot's radius."
  @spec contains?(Bot.t, Point.t) :: boolean
  def contains?(bot, loc) do
    if bot.radius < 0 do
      :ok = Logger.warn(
        "Bot #{bot.id} has a negative radius (#{bot.radius} meters)."
      )
      false
    else
      distance_from(bot, loc) <= bot.radius
    end
  end

  @spec is_visible_query(Queryable.t, User.t) :: Queryable.t
  def is_visible_query(queryable, user) do
    queryable
    |> Blocking.object_visible_query(user.id, :user_id)
    |> join(:left, [b, ...], s in Share,
            b.id == s.bot_id and s.user_id == ^user.id)
    |> where([b, ..., s],
             b.user_id == ^user.id or b.public or not is_nil(s.user_id))
  end

  @spec bump_update_time(Bot.t) :: :ok
  def bump_update_time(bot) do
    bot
    |> cast(%{updated_at: NaiveDateTime.utc_now()}, [:updated_at])
    |> Repo.update!
    :ok
  end

  @spec lat(Bot.t) :: float | nil
  def lat(%Bot{location: %Geo.Point{coordinates: {_, lat}}}), do: lat
  def lat(_), do: nil

  @spec lon(Bot.t) :: float | nil
  def lon(%Bot{location: %Geo.Point{coordinates: {lon, _}}}), do: lon
  def lon(_), do: nil

  defp maybe_filter_pending(queryable, false) do
    queryable
  end
  defp maybe_filter_pending(queryable, true) do
    queryable
    |> where(pending: false)
  end

  defp maybe_tidy_home_streams(%Bot{public: true} = bot, %{public: false}) do
    HomeStreamItem.delete_by_bot_ref_invisible(%{bot | public: false})
  end
  defp maybe_tidy_home_streams(_, _), do: :ok

end
