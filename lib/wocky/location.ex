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
    |> bots_with_events(jid, location)
    |> Enum.each(&trigger_bot_notification(jid, &1))
    :ok
  end

  defp get_followed_bots(jid) do
    jid(server: server) = jid
    Bot.followed_bots(server, jid)
  end

  defp bots_with_events(bots, jid, location) do
    bots |> Enum.reduce([], &check_for_event(&1, jid, location, &2))
  end

  defp check_for_event(bot_id, jid, location, acc) do
    if bot_id |> get_bot |> intersects?(location) do
      if check_for_enter_event(jid, bot_id) do
        [{bot_id, :enter} | acc]
      else
        acc
      end
    else
      if check_for_exit_event(jid, bot_id) do
        [{bot_id, :exit} | acc]
      else
        acc
      end
    end
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

  defp check_for_enter_event(jid, bot_id) do
    case lookup_bot_events(jid, bot_id) do
      [] -> true
      [%{event: "exit"}] -> true
      _ -> false
    end
  end

  defp check_for_exit_event(jid, bot_id) do
    case lookup_bot_events(jid, bot_id) do
      [] -> false
      [%{event: "enter"}] -> true
      _ -> false
    end
  end

  defp lookup_bot_events(jid, bot_id) do
    Schemata.select :all,
      from: :bot_event, in: :wocky_db.local_keyspace,
      where: %{jid: jid, bot: bot_id},
      limit: 1
  end

  defp trigger_bot_notification(jid, {bot, event}) do
    jid = Jid.to_binary(jid)
    Logger.info("User #{jid} #{event}ed the radius of bot #{bot.title}")
  end
end
