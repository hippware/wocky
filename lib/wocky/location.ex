defmodule Wocky.Location do
  @moduledoc "Interface for user location processing."

  use Exref, ignore: [__struct__: 0, __struct__: 1]
  use Wocky.Ejabberd
  alias Wocky.Location
  alias :wocky_db_user, as: User
  alias :wocky_db_bot, as: Bot
  require Logger

  defstruct [
    lat: :float,
    lon: :float,
    accuracy: :float
  ]

  @type location_tuple :: {float, float, float}


  @doc """
  Process a location change event for a user. The processing happens
  asynchronously and the function always returns `:ok`.
  """
  @spec user_location_changed(Ejabberd.jid, location_tuple) :: :ok
  def user_location_changed(jid, {lat, lon, accuracy}) do
    %Location{lat: lat, lon: lon, accuracy: accuracy}
    |> save_location(jid)
    |> check_for_bot_event(jid)
  end

  defp save_location(location, jid) do
    jid(user: user, server: server, resource: resource) = jid
    :ok = User.set_location(user, server, resource,
                            location.lat, location.lon, location.accuracy)
    location
  end

  defp check_for_bot_event(location, jid) do
    jid
    |> get_followed_bots
    |> bots_with_events(location)
    |> Enum.each(&trigger_bot_notification(jid, &1))
    :ok
  end

  defp get_followed_bots(jid) do
    jid(server: server) = jid
    Bot.followed_bots(server, jid)
  end

  defp bots_with_events(bots, location) do
    bots |> Enum.filter(&check_for_event(&1, location))
  end

  defp check_for_event(bot_id, location) do
    bot_id |> get_bot |> intersects?(location)
  end

  defp get_bot(bot_id) do
    case Bot.get(<<>>, bot_id) do
      :not_found -> nil
      bot -> bot
    end
  end

  defp intersects?(nil, _location), do: false
  defp intersects?(bot, location) do
    Geocalc.distance_between(bot, location) <= bot.radius
  end

  defp trigger_bot_notification(jid, bot) do
    jid = Jid.to_binary(jid)
    Logger.info("User #{jid} is within the radius of bot #{bot.shortname}")
  end
end
