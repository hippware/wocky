defmodule Wocky.Location do
  @moduledoc "Interface for user location processing."

  use Exref, ignore: [__struct__: 0, __struct__: 1, user_location_changed: 3]
  use Wocky.JID

  import Record, only: [defrecordp: 2, extract: 2]

  alias Wocky.Bot
  alias Wocky.EventHandler
  alias Wocky.Events.BotPerimeterEvent
  alias Wocky.User
  alias __MODULE__, as: Location

  require Logger
  require Record

  defrecordp :xmlel, extract(:xmlel, from_lib: "exml/include/exml.hrl")
  defrecordp :xmlcdata, extract(:xmlcdata, from_lib: "exml/include/exml.hrl")

  @type t :: %__MODULE__{
    lat: float,
    lon: float,
    accuracy: float
  }

  @enforce_keys [:lat, :lon]
  defstruct [
    lat: nil,
    lon: nil,
    accuracy: 0.0
  ]

  @type location_tuple :: {float, float, float}


  @doc """
  Process a location change event for a user. The processing happens
  asynchronously by default and the function always returns `:ok`.
  """
  @spec user_location_changed(JID.t, location_tuple, boolean) :: :ok
  def user_location_changed(jid, {lat, lon, accuracy}, async \\ true) do
    location = %Location{lat: lat, lon: lon, accuracy: accuracy}
    user = User.from_jid(jid)

    do_user_location_changed(user, location, async)
  end

  defp do_user_location_changed(user, location, true) do
    {:ok, _} = Task.start(fn () -> check_for_bot_events(user, location) end)
    {:ok, _} = Task.start(fn () -> update_bot_locations(user, location) end)
    :ok
  end
  defp do_user_location_changed(user, location, false) do
    check_for_bot_events(user, location)
    update_bot_locations(user, location)
    :ok
  end

  defp check_for_bot_events(user, location) do
    user
    |> User.set_location(location)
    |> User.get_subscribed_bots
    |> Enum.map(&:wocky_bot_util.get_id_from_jid(&1))
    |> bots_with_events(user, location)
    |> Enum.each(&trigger_bot_notification(user, &1))
  end

  defp update_bot_locations(user, location) do
    if Application.fetch_env!(:wocky, :enable_follow_me_updates) do
      user
      |> owned_bots_with_follow_me
      |> Enum.each(&Bot.set_location(&1, location))
    end
  end

  defp bots_with_events(bots, user, location) do
    Enum.reduce(bots, [], &check_for_event(&1, user, location, &2))
  end

  defmacrop log_check_result(user, bot_id, result) do
    quote do
      :ok = Logger.debug("""
      User #{unquote(user).user} #{unquote(result)} the perimeter \
      of #{unquote(bot_id)}\
      """)
    end
  end

  defp check_for_event(bot_id, user, location, acc) do
    :ok = Logger.debug("""
    Checking user #{user.user} for collision with bot #{bot_id} \
    at location (#{location.lat},#{location.lon})...\
    """)
    bot = Bot.get(bot_id)
    if is_nil(bot) do
      :ok = Logger.warn("Could not find bot for ID #{bot_id}")
      acc
    else
      bot
      |> unless_owner(user)
      |> intersects?(location)
      |> handle_intersection(user, bot, acc)
    end
  end

  defp unless_owner(bot, user) do
    owner_jid = JID.from_binary!(bot.owner)

    # Don't check bots that are owned by the user
    if JID.bare_equal?(owner_jid, User.to_jid(user)) do
      :ok = Logger.debug(
        "Skipping bot #{bot.id} since it is owned by #{user.user}"
      )
      nil
    else
      bot
    end
  end

  defp intersects?(nil, _location), do: false
  defp intersects?(bot, location) do
    radius = (bot.radius / 1000.0) # Bot radius is stored as millimeters

    if radius < 0 do
      :ok = Logger.warn(
        "Bot #{bot.id} has a negative radius (#{radius} meters); skipping."
      )
      false
    else
      distance = Geocalc.distance_between(Map.from_struct(bot),
                                          Map.from_struct(location))
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
      User.add_bot_event(user, bot_id, :enter)
      [{bot, :enter} | acc]
    else
      log_check_result(user, bot_id, "is within")
      acc
    end
  end
  defp handle_intersection(false, user, %Bot{id: bot_id} = bot, acc) do
    if check_for_exit_event(user, bot_id) do
      log_check_result(user, bot_id, "has left")
      User.add_bot_event(user, bot_id, :exit)
      [{bot, :exit} | acc]
    else
      log_check_result(user, bot_id, "is outside of")
      acc
    end
  end

  defp check_for_enter_event(user, bot_id) do
    case User.get_last_bot_event(user, bot_id) do
      [] -> true
      [%{event: "exit"}] -> true
      _ -> false
    end
  end

  defp check_for_exit_event(user, bot_id) do
    case User.get_last_bot_event(user, bot_id) do
      [] -> false
      [%{event: "enter"}] -> true
      _ -> false
    end
  end

  defp trigger_bot_notification(user, {bot, event}) do
    jid = User.to_jid_string(user)
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
    expiry > :wocky_db.now_to_timestamp(:os.timestamp)
  end
  defp following_me?(_), do: false
end
