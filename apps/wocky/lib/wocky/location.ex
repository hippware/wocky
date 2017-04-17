defmodule Wocky.Location do
  @moduledoc "Interface for user location processing."

  use Wocky.Repo.Model
  use Wocky.JID

  alias Wocky.Bot
  alias Wocky.BotEvent
  alias Wocky.EventHandler
  alias Wocky.Events.BotPerimeterEvent
  alias Wocky.Repo.Timestamp
  alias Wocky.User
  alias __MODULE__, as: Location

  require Logger

  @foreign_key_type :binary_id
  @primary_key false
  schema "locations" do
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

  @doc ""
  @spec check_for_bot_events(User.t, float, float) :: :ok
  def check_for_bot_events(user, lat, lon) do
    maybe_do_async fn ->
      user
      |> User.get_subscribed_bots
      |> Enum.map(&Bot.get_id_from_jid(&1))
      |> bots_with_events(user, lat, lon)
      |> Enum.each(&trigger_bot_notification(user, &1))
    end
  end

  @doc ""
  @spec update_bot_locations(User.t, float, float) :: :ok
  def update_bot_locations(user, lat, lon) do
    if Application.fetch_env!(:wocky, :enable_follow_me_updates) do
      maybe_do_async fn ->
        user
        |> owned_bots_with_follow_me
        |> Enum.each(&Bot.set_location(&1, lat, lon))
      end
    end
  end

  defp maybe_do_async(fun) do
    task = Task.async(fun)
    unless Application.fetch_env!(:wocky, :async_location_processing) do
      Task.await(task)
    end
  end

  defp bots_with_events(bots, user, lat, lon) do
    Enum.reduce(bots, [], &check_for_event(&1, user, lat, lon, &2))
  end

  defmacrop log_check_result(user, bot_id, result) do
    quote do
      :ok = Logger.debug("""
      User #{unquote(user).id} #{unquote(result)} the perimeter \
      of #{unquote(bot_id)}\
      """)
    end
  end

  defp check_for_event(bot_id, user, lat, lon, acc) do
    :ok = Logger.debug("""
    Checking user #{user.id} for collision with bot #{bot_id} \
    at location (#{lat},#{lon})...\
    """)
    bot = Bot.get(bot_id)
    if is_nil(bot) do
      :ok = Logger.warn("Could not find bot for ID #{bot_id}")
      acc
    else
      bot
      |> unless_owner(user)
      |> intersects?(lat, lon)
      |> handle_intersection(user, bot, acc)
    end
  end

  defp unless_owner(bot, user) do
    # Don't check bots that are owned by the user
    if bot.user_id == user.id do
      :ok = Logger.debug(
        "Skipping bot #{bot.id} since it is owned by #{user.id}"
      )
      nil
    else
      bot
    end
  end

  defp intersects?(nil, _lat, _lon), do: false
  defp intersects?(bot, lat, lon) do
    radius = (bot.radius / 1000.0) # Bot radius is stored as millimeters

    if radius < 0 do
      :ok = Logger.warn(
        "Bot #{bot.id} has a negative radius (#{radius} meters); skipping."
      )
      false
    else
      distance = Geocalc.distance_between(Map.from_struct(bot),
                                          %{lat: lat, lon: lon})
      intersects = distance <= radius
      :ok = Logger.debug("""
      The distance of #{distance} meters is \
      #{if intersects, do: "within", else: "outside"} the radius of bot \
      #{bot.id} (#{radius} meters)\
      """)
      intersects
    end
  end

  defp handle_intersection(true, user, %Bot{id: bot_id} = bot, acc) do
    if check_for_enter_event(user, bot_id) do
      log_check_result(user, bot_id, "has entered")
      BotEvent.add_event(user.id, bot_id, :enter)
      [{bot, :enter} | acc]
    else
      log_check_result(user, bot_id, "is within")
      acc
    end
  end
  defp handle_intersection(false, user, %Bot{id: bot_id} = bot, acc) do
    if check_for_exit_event(user, bot_id) do
      log_check_result(user, bot_id, "has left")
      BotEvent.add_event(user.id, bot_id, :exit)
      [{bot, :exit} | acc]
    else
      log_check_result(user, bot_id, "is outside of")
      acc
    end
  end

  defp check_for_enter_event(user, bot_id) do
    case BotEvent.get_last_event_type(user.id, bot_id) do
      nil -> true
      :exit -> true
      :enter -> false
    end
  end

  defp check_for_exit_event(user, bot_id) do
    case BotEvent.get_last_event_type(user.id, bot_id) do
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

      EventHandler.broadcast(event)
    end
  end

  defp owned_bots_with_follow_me(user) do
    user
    |> User.get_owned_bots
    |> Enum.filter(&following_me?(&1))
  end

  defp following_me?(%Bot{follow_me: true, follow_me_expiry: expiry}) do
    !Timestamp.expired?(expiry)
  end
  defp following_me?(_), do: false
end
