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
    field :tags,             {:array, :string}

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
                  :alerts, :follow_me, :follow_me_expiry, :tags]
  @required_fields [:id, :server, :user_id, :title, :lat, :lon, :radius]

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
  def get_id_from_jid(jid(lresource: @bot_prefix <> id)), do: id
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
  # Databse interaction

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
    |> update_change(:lat, &GeoUtils.normalize_latitude/1)
    |> update_change(:lon, &GeoUtils.normalize_longitude/1)
    |> validate_number(:radius, greater_than: 0)
    |> put_change(:pending, false)
    |> unique_constraint(:shortname)
    |> foreign_key_constraint(:user_id)
  end

  @spec insert(map) :: {:ok, t} | {:error, any}
  def insert(params), do: do_update(%Bot{}, params, &Repo.insert/1)

  @spec update(t, map) :: {:ok, t} | {:error, any}
  def update(bot, params), do: do_update(bot, params, &Repo.update/1)

  defp do_update(struct, params, op) do
    changeset = changeset(struct, params)
    case op.(changeset) do
      {:ok, bot} = result ->
        Index.update(:bot, bot.id, changeset.changes)
        result

      {:error, _} = error ->
        error
    end
  end

  @spec delete(t) :: :ok
  def delete(bot) do
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
    bot = Repo.preload(bot, [:subscribers])

    temp_subscribers =
      bot
      |> TempSubscription.get_all
      |> Enum.map(fn %TempSubscription{user: user, resource: resource} ->
        %User{user | resource: resource}
      end)

    [bot.subscribers, temp_subscribers]
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
end
