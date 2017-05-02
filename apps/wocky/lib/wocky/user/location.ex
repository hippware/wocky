defmodule Wocky.User.Location do
  @moduledoc "Interface for user location processing."

  use Wocky.Repo.Model
  use Wocky.JID

  alias Wocky.Bot
  alias Wocky.Events.BotPerimeterEvent
  alias Wocky.GeoUtils
  alias Wocky.User
  alias Wocky.User.BotEvent
  alias __MODULE__, as: Location

  require Logger

  @foreign_key_type :binary_id
  @primary_key false
  schema "user_locations" do
    field :user_id,   :binary_id, null: false, primary_key: true
    field :resource,  :string, null: false, primary_key: true
    field :lat,       :float, null: false
    field :lon,       :float, null: false
    field :accuracy,  :float

    timestamps()

    belongs_to :user, User, define_field: false
  end

  @type location_tuple :: {float, float, float}
  @type t :: %Location{
    user_id: User.id,
    resource: User.resource,
    lat: float,
    lon: float,
    accuracy: float
  }

  @doc "Store a user location datapoint"
  @spec insert(User.t, User.resource, float, float, float)
    :: {:ok, t} | {:error, any}
  def insert(user, resource, lat, lon, accuracy) do
    data = %{resource: resource, lat: lat, lon: lon, accuracy: accuracy}

    user
    |> build_assoc(:locations)
    |> changeset(data)
    |> Repo.insert
  end

  def changeset(struct, params) do
    struct
    |> cast(params, [:resource, :lat, :lon, :accuracy])
    |> validate_required([:resource, :lat, :lon, :accuracy])
    |> validate_number(:accuracy, greater_than_or_equal_to: 0)
    |> update_change(:lat, &GeoUtils.normalize_latitude/1)
    |> update_change(:lon, &GeoUtils.normalize_longitude/1)
  end

  @doc ""
  @spec check_for_bot_events(t) :: t
  def check_for_bot_events(%Location{user: user} = loc) do
    maybe_do_async fn ->
      user
      |> User.get_subscribed_bots
      |> bots_with_events(user, loc)
      |> Enum.each(&trigger_bot_notification(user, &1))
    end

    loc
  end

  @doc ""
  @spec update_bot_locations(t) :: t
  def update_bot_locations(%Location{user: user} = loc) do
    if Application.fetch_env!(:wocky, :enable_follow_me_updates) do
      maybe_do_async fn ->
        user
        |> User.get_owned_bots_with_follow_me
        |> Enum.each(&Bot.set_location(&1, loc.lat, loc.lon, loc.accuracy))
      end
    end

    loc
  end

  defp maybe_do_async(fun) do
    if Application.fetch_env!(:wocky, :async_location_processing) do
      Task.async(fun)
    else
      fun.()
    end
  end

  defp bots_with_events(bots, user, loc) do
    Enum.reduce(bots, [], &check_for_event(&1, user, loc, &2))
  end

  defmacrop log_check_result(user, bot, result) do
    quote do
      :ok = Logger.debug("""
      User #{unquote(user).id} #{unquote(result)} the perimeter \
      of #{unquote(bot).id}\
      """)
    end
  end

  defp check_for_event(bot, user, loc, acc) do
    :ok = Logger.debug("""
    Checking user #{user.id} for collision with bot #{bot.id} \
    at location (#{loc.lat},#{loc.lon})...\
    """)
    # Don't check bots that are owned by the user
    if bot.user_id == user.id do
      :ok = Logger.debug(
        "Skipping bot #{bot.id} since it is owned by #{user.id}"
      )
      acc
    else
      bot
      |> intersects?(loc)
      |> handle_intersection(user, bot, acc)
    end
  end

  defp intersects?(bot, loc) do
    radius = (bot.radius / 1000.0) # Bot radius is stored as millimeters

    if radius < 0 do
      :ok = Logger.warn(
        "Bot #{bot.id} has a negative radius (#{radius} meters); skipping."
      )
      false
    else
      distance = Geocalc.distance_between(Map.from_struct(bot),
                                          Map.from_struct(loc))
      intersects = distance <= radius
      :ok = Logger.debug("""
      The distance of #{distance} meters is \
      #{if intersects, do: "within", else: "outside"} the radius of bot \
      #{bot.id} (#{radius} meters)\
      """)
      intersects
    end
  end

  defp handle_intersection(true, user, bot, acc) do
    if should_generate_enter_event(user, bot) do
      log_check_result(user, bot, "has entered")
      BotEvent.insert(user, bot, :enter)
      [{bot, :enter} | acc]
    else
      log_check_result(user, bot, "is within")
      acc
    end
  end
  defp handle_intersection(false, user, bot, acc) do
    if should_generate_exit_event(user, bot) do
      log_check_result(user, bot, "has left")
      BotEvent.insert(user, bot, :exit)
      [{bot, :exit} | acc]
    else
      log_check_result(user, bot, "is outside of")
      acc
    end
  end

  defp should_generate_enter_event(user, bot) do
    case BotEvent.get_last_event_type(user.id, bot.id) do
      nil -> true
      :exit -> true
      :enter -> false
    end
  end

  defp should_generate_exit_event(user, bot) do
    case BotEvent.get_last_event_type(user.id, bot.id) do
      nil -> false
      :exit -> false
      :enter -> true
    end
  end

  defp trigger_bot_notification(user, {bot, event}) do
    jid = user.username |> JID.make(user.server) |> JID.to_binary
    :ok = Logger.info("User #{jid} #{event}ed the perimeter of bot #{bot.id}")

    if Application.fetch_env!(:wocky, :enable_bot_event_notifications) do
      event = %BotPerimeterEvent{
        user: user,
        bot: bot,
        event: event
      }

      event_handler().broadcast(event)
    end
  end

  defp event_handler, do: Application.fetch_env!(:wocky, :event_handler)
end
