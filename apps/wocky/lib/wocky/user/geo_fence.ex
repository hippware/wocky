defmodule Wocky.User.GeoFence do
  @moduledoc false

  import Ecto.Query

  alias Wocky.Bot
  alias Wocky.Repo
  alias Wocky.User
  alias Wocky.User.{BotEvent, Location}

  require Logger

  @spec exit_bot(User.t(), Bot.t(), String.t()) :: :ok
  def exit_bot(user, bot, reason) do
    config = get_config()
    last_event = BotEvent.get_last_event_type(user, bot)

    if inside?(last_event) do
      BotEvent.insert_system(user, bot, :exit, reason)
      Bot.depart(bot, user, config.enable_notifications)
    end

    :ok
  end

  @spec exit_all_bots(User.t(), String.t()) :: :ok
  def exit_all_bots(user, reason) do
    user.id
    |> BotEvent.get_last_events()
    |> Enum.each(fn last_event ->
      if inside?(last_event.event) do
        last_event = Repo.preload(last_event, :bot)
        BotEvent.insert_system(user, last_event.bot, :exit, reason)
      end
    end)

    Bot.depart_all_quietly(user)
  end

  defp inside?(last_event_type),
    do: Enum.member?([:enter, :transition_in], last_event_type)

  @doc false
  @spec check_for_bot_event(Bot.t(), Location.t(), User.t()) :: Location.t()
  def check_for_bot_event(bot, loc, user) do
    config = get_config(debounce: false)

    if location_valid?(loc, config) do
      bot
      |> check_for_event(user, loc, config, [])
      |> Enum.each(&process_bot_event(&1, config))
    end

    loc
  end

  @doc false
  @spec check_for_bot_events(Location.t(), User.t()) :: Location.t()
  def check_for_bot_events(%Location{} = loc, user) do
    config = get_config()

    if location_valid?(loc, config) do
      maybe_do_async(
        fn ->
          user
          |> User.get_guest_subscriptions()
          |> check_for_events(user, loc, config)
          |> Enum.each(&process_bot_event(&1, config))
        end,
        config
      )
    end

    loc
  end

  defp get_config(opts \\ []) do
    Confex.fetch_env!(:wocky, __MODULE__)
    |> Keyword.merge(opts)
    |> Enum.into(%{})
  end

  defp location_valid?(%Location{accuracy: accuracy}, config) do
    accuracy <= config.max_accuracy_threshold
  end

  defp maybe_do_async(fun, %{async_processing: true}), do: Task.start_link(fun)

  defp maybe_do_async(fun, _), do: fun.()

  defp check_for_events(bots, user, loc, config) do
    Enum.reduce(bots, [], &check_for_event(&1, user, loc, config, &2))
  end

  defp check_for_event(bot, user, loc, config, acc) do
    bot
    |> Bot.contains?(Map.from_struct(loc))
    |> maybe_set_exit_timer(user, bot, loc, config)
    |> handle_intersection(user, bot, loc, config, acc)
  end

  defp maybe_set_exit_timer(false, _, _, _, _), do: false

  defp maybe_set_exit_timer(true, _, _, _, %{visit_timeout_enabled: false}),
    do: true

  defp maybe_set_exit_timer(true, user, bot, loc, config) do
    dawdle_event = %{
      user_id: user.id,
      resource: loc.resource,
      bot_id: bot.id,
      loc_id: loc.id
    }

    timeout = config.visit_timeout_seconds * 1000

    Dawdle.call_after(&visit_timeout/1, dawdle_event, timeout)

    true
  end

  defp handle_intersection(inside?, user, bot, loc, config, acc) do
    event = BotEvent.get_last_event(user.id, bot.id)

    case user_state_change(inside?, event, bot, loc, config) do
      :no_change ->
        acc

      {:roll_back, old_state} ->
        BotEvent.insert(user, loc.resource, bot, loc, old_state)
        acc

      new_state ->
        new_event = BotEvent.insert(user, loc.resource, bot, loc, new_state)
        [{user, bot, new_event} | acc]
    end
  end

  defp user_state_change(true, nil, _, _, %{debounce: true}), do: :transition_in
  defp user_state_change(true, nil, _, _, %{debounce: false}), do: :enter

  defp user_state_change(true, be, _bot, loc, config) do
    case be.event do
      :exit ->
        maybe_enter(loc, config)

      :enter ->
        :no_change

      :timeout ->
        :reactivate

      :deactivate ->
        maybe_enter(loc, config)

      :reactivate ->
        :no_change

      :transition_out ->
        {:roll_back, :enter}

      :transition_in ->
        debounce_secs = config.enter_debounce_seconds

        if debounce_complete?(loc, be.created_at, config, debounce_secs) do
          :enter
        else
          :no_change
        end
    end
  end

  defp user_state_change(false, nil, _bot, _loc, _config), do: :no_change

  defp user_state_change(false, be, bot, loc, config) do
    case be.event do
      :exit ->
        :no_change

      :enter ->
        maybe_exit(loc, bot, config)

      :timeout ->
        :deactivate

      :deactivate ->
        :no_change

      :reactivate ->
        maybe_exit(loc, bot, config)

      :transition_in ->
        {:roll_back, :exit}

      :transition_out ->
        debounce_secs = config.exit_debounce_seconds

        if debounce_complete?(loc, be.created_at, config, debounce_secs) do
          :exit
        else
          :no_change
        end
    end
  end

  defp maybe_enter(_, %{debounce: false}), do: :enter

  defp maybe_enter(loc, config) do
    if moving_slowly?(loc, config) do
      :enter
    else
      :transition_in
    end
  end

  defp maybe_exit(_, _, %{debounce: false}), do: :exit

  defp maybe_exit(loc, bot, config) do
    if too_far?(bot, loc, config) || moving_slowly?(loc, config) do
      :exit
    else
      :transition_out
    end
  end

  defp moving_slowly?(loc, config) do
    loc.is_moving == false ||
      (!is_nil(loc.speed) && loc.speed >= 0 &&
         loc.speed <= config.max_slow_speed)
  end

  defp too_far?(bot, loc, config) do
    Bot.distance_from(bot, Map.from_struct(loc)) > config.max_exit_distance
  end

  defp debounce_complete?(_, _, %{debounce: false}, _), do: true

  defp debounce_complete?(loc, ts, config, debounce_secs) do
    moving_slowly?(loc, config) || debounce_expired?(ts, debounce_secs)
  end

  defp debounce_expired?(ts, debounce_secs) do
    Timex.diff(Timex.now(), ts, :seconds) >= debounce_secs
  end

  defp process_bot_event({user, bot, be}, config) do
    maybe_visit_bot(be.event, user, bot, notify?(be, config))
  end

  defp notify?(be, %{enable_notifications: enabled?} = config) do
    enabled? && default_notify(be.event) && !stale?(be, config)
  end

  defp stale?(%BotEvent{occurred_at: ts}, config) do
    Timex.diff(Timex.now(), ts, :seconds) >= config.stale_update_seconds
  end

  defp maybe_visit_bot(:enter, user, bot, notify),
    do: Bot.visit(bot, user, notify)

  defp maybe_visit_bot(:reactivate, user, bot, notify),
    do: Bot.visit(bot, user, notify)

  defp maybe_visit_bot(:exit, user, bot, notify),
    do: Bot.depart(bot, user, notify)

  defp maybe_visit_bot(:timeout, user, bot, notify),
    do: Bot.depart(bot, user, notify)

  defp maybe_visit_bot(_, _, _, _), do: :ok

  defp default_notify(:enter), do: true
  defp default_notify(:reactivate), do: false
  defp default_notify(:exit), do: true
  defp default_notify(:timeout), do: false
  defp default_notify(_), do: false

  def visit_timeout(%{
        user_id: user_id,
        resource: resource,
        bot_id: bot_id,
        loc_id: loc_id
      }) do
    case latest_loc(user_id) do
      %{id: ^loc_id} ->
        do_visit_timeout(user_id, resource, bot_id)

      _ ->
        :ok
    end
  end

  defp do_visit_timeout(user_id, resource, bot_id) do
    config = get_config()
    user = Repo.get(User, user_id)
    bot = Bot.get(bot_id)

    if user && bot do
      if Bot.subscription(bot, user) == :visitor do
        new_event = BotEvent.insert(user, resource, bot, :timeout)
        process_bot_event({user, bot, new_event}, config)
      end
    end
  end

  defp latest_loc(user_id) do
    Location
    |> where(user_id: ^user_id)
    |> order_by(desc: :created_at)
    |> limit(1)
    |> Repo.one()
  end
end
