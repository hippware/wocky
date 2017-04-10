defmodule Wocky.Location do
  @moduledoc "Interface for user location processing."

  alias Wocky.Bot
  alias Wocky.JID
  alias Wocky.Repo.Timestamp
  alias Wocky.User
  alias __MODULE__, as: Location

  require Logger

  @enforce_keys [:lat, :lon]
  defstruct [
    lat: nil,
    lon: nil,
    accuracy: 0.0
  ]

  @type t :: %Location{
    lat: float,
    lon: float,
    accuracy: float
  }

  @type location_tuple :: {float, float, float}

  @doc ""
  @spec check_for_bot_events(User.t, t) :: {:ok, User.t}
  def check_for_bot_events(user, location) do
    maybe_do_async fn ->
      user
      |> User.get_subscribed_bots
      |> Enum.map(&:wocky_bot_util.get_id_from_jid(&1))
      |> bots_with_events(user, location)
      |> Enum.each(&trigger_bot_notification(user, &1))
    end
  end

  @doc ""
  @spec update_bot_locations(User.t, t) :: {:ok, User.t}
  def update_bot_locations(user, location) do
    if Application.fetch_env!(:wocky, :enable_follow_me_updates) do
      maybe_do_async fn ->
        user
        |> owned_bots_with_follow_me
        |> Enum.each(&Bot.set_location(&1, location))
      end
    end
  end

  defp maybe_do_async(fun) do
    {:ok, pid} = Task.start(fun)
    unless Application.fetch_env!(:wocky, :async_location_processing) do
      Task.await(pid)
    end
  end

  defp bots_with_events(bots, user, location) do
    Enum.reduce(bots, [], &check_for_event(&1, user, location, &2))
  end

  defmacrop log_check_result(user, bot_id, result) do
    quote do
      :ok = Logger.debug("""
      User #{unquote(user).id} #{unquote(result)} the perimeter \
      of #{unquote(bot_id)}\
      """)
    end
  end

  defp check_for_event(bot_id, user, location, acc) do
    :ok = Logger.debug("""
    Checking user #{user.id} for collision with bot #{bot_id} \
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
    owner_jid = JID.from_binary(bot.owner)
    user_jid = JID.make(user.username, user.server)

    # Don't check bots that are owned by the user
    if JID.bare_equal?(owner_jid, user_jid) do
      :ok = Logger.debug(
        "Skipping bot #{bot.id} since it is owned by #{user.id}"
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
    jid = JID.to_binary(JID.make(user.username, user.server))
    :ok = Logger.info("User #{jid} #{event}ed the perimeter of bot #{bot.id}")

    if Application.fetch_env!(:wocky, :enable_bot_event_notifications) do
      owner_jid = JID.from_binary(bot.owner)
      :ok = send_notification(owner_jid, user, bot, event)
      :ok = send_push_notification(owner_jid, user, bot, event)
    end
  end

  defp send_push_notification(owner_jid, user, bot, event) do
    user_jid = JID.make(user.username, user.server)
    result = :wocky_notification_handler.notify_bot_event(
                  owner_jid, user_jid, bot.title, event)
    case result do
      :ok -> :ok
      {:error, reason} ->
        Logger.error("""
        Failed to send push notification to #{JID.to_binary(owner_jid)}: \
        #{inspect(reason)}\
        """)
    end
  end

  defp send_notification(_owner_jid, _user, _bot, _event) do
    # :ejabberd_router.route(Ejabberd.make_jid!("", :wocky_app.server),
    #                        :jid.to_bare(owner_jid),
    #                        bot_notification_stanza(owner_jid, user,
    #                                                bot, event))
  end

  # defp bot_notification_stanza(owner_jid, user, bot, event) do
  #   user_jid_str = User.to_bare_jid_string(user)
  #   bot_jid_str = Bot.to_jid_string(bot)
  #   xmlel(name: "message",
  #         attrs: [
  #           {"from", :wocky_app.server},
  #           {"to", :jid.to_binary(owner_jid)},
  #           {"type", "headline"}
  #         ],
  #         children: [
  #           xmlel(name: "bot", attrs: [{"xmlns", "hippware.com/hxep/bot"}],
  #                 children: [
  #                   xmlel(name: "jid", children: [
  #                           xmlcdata(content: bot_jid_str)
  #                         ]),
  #                   xmlel(name: "id", children: [
  #                           xmlcdata(content: bot.id)
  #                         ]),
  #                   xmlel(name: "server", children: [
  #                           xmlcdata(content: bot.server)
  #                         ]),
  #                   xmlel(name: "action", children: [
  #                           xmlcdata(content: to_string(event))
  #                         ]),
  #                   xmlel(name: "user-jid", children: [
  #                           xmlcdata(content: user_jid_str)
  #                         ])
  #                 ])
  #         ])
  # end

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
