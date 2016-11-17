defmodule Wocky.Location do
  @moduledoc "Interface for user location processing."

  use Exref, ignore: [__struct__: 0, __struct__: 1, user_location_changed: 3]
  use Wocky.Ejabberd
  alias Wocky.Location
  alias Wocky.User
  alias Wocky.Bot
  require Logger

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
  asynchronously and the function always returns `:ok`.
  """
  def user_location_changed(user, location, async \\ true)

  @spec user_location_changed(Ejabberd.jid, location_tuple, boolean) :: :ok
  def user_location_changed(jid, {lat, lon, accuracy}, async) do
    location = %Location{lat: lat, lon: lon, accuracy: accuracy}
    user = User.from_jid(jid)

    user_location_changed(user, location, async)
  end

  @spec user_location_changed(User.t, Location.t, boolean) :: :ok
  def user_location_changed(user, location, true) do
    {:ok, _} = Task.start(__MODULE__, :check_for_bot_events, [user, location])
    {:ok, _} = Task.start(__MODULE__, :update_bots_with_follow_me,
                          [user, location])
    :ok
  end
  def user_location_changed(user, location, false) do
    check_for_bot_events(user, location)
    update_bots_with_follow_me(user, location)
    :ok
  end

  defp check_for_bot_events(user, location) do
    user
    |> User.set_location(location)
    |> User.get_followed_bots
    |> bots_with_events(user, location)
    |> Enum.each(&trigger_bot_notification(user, &1))
  end

  defp update_bots_with_follow_me(user, location) do
    user
    |> owned_bots_with_follow_me
    |> Enum.each(&Bot.set_location(&1, location))
  end

  defp bots_with_events(bots, user, location) do
    bots |> Enum.reduce([], &check_for_event(&1, user, location, &2))
  end

  defp check_for_event(bot_id, user, location, acc) do
    :ok = Logger.debug(
      "Checking #{bot_id} for collision at #{inspect(location)}...")
    if bot_id |> Bot.get |> intersects?(location) do
      :ok = Logger.debug("User is within the perimeter of #{bot_id}")
      if check_for_enter_event(user, bot_id) do
        User.add_bot_event(user, bot_id, :enter)
        [{bot_id, :enter} | acc]
      else
        acc
      end
    else
      :ok = Logger.debug("User is outside of the perimeter of #{bot_id}")
      if check_for_exit_event(user, bot_id) do
        User.add_bot_event(user, bot_id, :exit)
        [{bot_id, :exit} | acc]
      else
        acc
      end
    end
  end

  defp intersects?(nil, _location), do: false
  defp intersects?(bot, location) do
    Geocalc.distance_between(Map.from_struct(bot),
                             Map.from_struct(location)) <= bot.radius
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

  defp trigger_bot_notification(user, {bot_id, event}) do
    jid = User.to_jid_string(user)
    :ok = Logger.info("User #{jid} #{event}ed the perimeter of bot #{bot_id}")

    jid = User.to_jid(user)
    :wocky_notification_handler.notify_bot_event(jid, bot_id, event)
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
