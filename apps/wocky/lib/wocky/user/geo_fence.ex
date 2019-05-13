defmodule Wocky.User.GeoFence do
  @moduledoc false

  alias Wocky.{Bot, Repo, User}
  alias Wocky.User.{BotEvent, Location}

  require Logger

  @spec save_locations? :: boolean
  def save_locations? do
    get_config().save_locations
  end

  @spec exit_bot(User.t(), Bot.t(), String.t()) :: :ok
  def exit_bot(user, bot, reason) do
    config = get_config()
    last_event = BotEvent.get_last_event_type(user.id, bot.id)

    if inside?(last_event) do
      _ = BotEvent.insert_system(user, bot, :exit, reason)
      Bot.depart(bot, user, config.enable_notifications)
    end

    :ok
  end

  defp inside?(last_event_type),
    do: Enum.member?([:enter, :transition_in], last_event_type)

  @doc false
  def check_for_bot_event(bot, loc, user) do
    events = BotEvent.get_last_events(user.id)

    check_for_bot_event(bot, loc, user, events)
  end

  @doc false
  @spec check_for_bot_event(Bot.t(), Location.t(), User.t(), map())
        :: Location.t()
  def check_for_bot_event(bot, loc, user, events) do
    config = get_config(debounce: false)

    new_events =
      if should_process?(loc, config) do
        bot
        |> check_for_event(user, loc, events, config, [])
        |> process_bot_events(user, config)
      else
        []
      end

    {:ok, loc, merge_new_events(events, new_events)}
  end

  @doc false
  def check_for_bot_events(%Location{} = loc, user) do
    subs = User.get_subscriptions(user)
    events = BotEvent.get_last_events(user.id)

    check_for_bot_events(loc, user, subs, events)
  end

  @doc false
  @spec check_for_bot_events(Location.t(), User.t(), [Bot.t()], map())
        :: Location.t()
  def check_for_bot_events(%Location{} = loc, user, subs, events) do
    config = get_config()

    new_events =
      if should_process?(loc, config) do
        subs
        |> check_for_events(user, loc, events, config)
        |> process_bot_events(user, config)
      else
        []
      end

    {:ok, loc, merge_new_events(events, new_events)}
  end

  defp merge_new_events(events, new_events) do
    Enum.reduce(
      new_events,
      events,
      fn {_, bot, event}, acc ->
        Map.put(acc, bot.id, event)
      end
    )
  end

  @doc false
  @spec get_config(Keyword.t()) :: map()
  def get_config(opts \\ []) do
    Confex.fetch_env!(:wocky, __MODULE__)
    |> Keyword.merge(opts)
    |> Enum.into(%{})
  end

  @doc false
  @spec should_process?(Location.t(), map()) :: boolean()
  def should_process?(%Location{accuracy: accuracy}, config),
    do: accuracy <= config.max_accuracy_threshold

  # defp maybe_do_async(fun, %{async_processing: true}) do
  #   {:ok, _} = Task.start_link(fun)
  #   :ok
  # end

  # defp maybe_do_async(fun, _) do
  #   fun.()
  #   :ok
  # end

  defp check_for_events(bots, user, loc, events, config) do
    Enum.reduce(bots, [], &check_for_event(&1, user, loc, events, config, &2))
  end

  defp check_for_event(bot, user, loc, events, config, acc) do
    event = Map.get(events, bot.id)

    bot
    |> Bot.contains?(Map.from_struct(loc))
    |> handle_intersection(user, bot, event, loc, config, acc)
  end

  defp handle_intersection(inside?, user, bot, event, loc, config, acc) do
    case user_state_change(inside?, event, bot, loc, config) do
      :no_change ->
        acc

      {:roll_back, old_state} ->
        new_event = BotEvent.new(user, loc.device, bot, loc, old_state)
        [{:old, bot, new_event} | acc]

      new_state ->
        new_event = BotEvent.new(user, loc.device, bot, loc, new_state)
        [{:new, bot, new_event} | acc]
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

  defp process_bot_events(events, user, config) do
    bes = Enum.map(events, fn {_, _, be} -> be end)
    Repo.insert_all(BotEvent, bes)

    Enum.each(events, &process_bot_event(&1, user, config))

    events
  end

  defp process_bot_event({:new, bot, be}, user, config) do
    maybe_visit_bot(be.event, user, bot, notify?(be, config))
  end

  defp process_bot_event(_event, _user, _config), do: :ok

  defp notify?(be, %{enable_notifications: enabled?} = config) do
    enabled? && default_notify(be.event) && !stale?(be, config)
  end

  defp stale?(%{occurred_at: ts}, config) do
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
end
