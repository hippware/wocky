defmodule Wocky.User.GeoFence do
  @moduledoc false

  import Ecto.Query

  alias Wocky.Bot
  alias Wocky.Repo
  alias Wocky.User
  alias Wocky.User.{BotEvent, Location}

  require Logger

  @doc false
  @spec check_for_bot_events(Location.t(), User.t(), User.resource()) ::
          Location.t()
  def check_for_bot_events(%Location{} = loc, user, resource) do
    user = %User{user | resource: resource}

    maybe_do_async(fn ->
      user
      |> User.get_guest_subscriptions()
      |> check_for_events(user, loc)
      |> Enum.each(&process_bot_event/1)
    end)

    loc
  end

  @spec check_for_bot_event(Bot.t(), Location.t(), User.t(), User.resource()) ::
  :ok
  def check_for_bot_event(bot, loc, user, resource) do
    bot
    |> check_for_event(%User{user | resource: resource}, loc, false, [])
    |> Enum.each(&process_bot_event/1)
  end

  defp maybe_do_async(fun) do
    if Application.fetch_env!(:wocky, :async_location_processing) do
      Task.async(fun)
    else
      fun.()
    end
  end

  defp check_for_events(bots, user, loc) do
    Enum.reduce(bots, [], &check_for_event(&1, user, loc, true, &2))
  end

  defp check_for_event(bot, user, loc, debounce, acc) do :ok =
      Logger.debug(fn ->
        """
        Checking user #{user.id} for collision with bot #{bot.id} \
        at location (#{loc.lat},#{loc.lon})...\
        """
      end)

    bot
    |> Bot.contains?(Map.from_struct(loc))
    |> maybe_set_exit_timer(user, bot, loc)
    |> handle_intersection(user, bot, loc, debounce, acc)
  end

  defp maybe_set_exit_timer(false, _, _, _), do: false

  defp maybe_set_exit_timer(true, user, bot, loc) do
    if Confex.get_env(:wocky, :visit_timeout_enabled) do
      dawdle_event = %{
        user_id: user.id,
        resource: user.resource,
        bot_id: bot.id,
        loc_id: loc.id
      }

      timeout = Confex.get_env(:wocky, :visit_timeout_seconds)

      Dawdle.call_after(&visit_timeout/1, dawdle_event, timeout * 1000)
    end

    true
  end

  defp handle_intersection(inside?, user, bot, loc, debounce, acc) do
    event = BotEvent.get_last_event(user.id, user.resource, bot.id)

    case user_state_change(inside?, event, debounce) do
      :no_change ->
        acc

      :roll_back ->
        Repo.delete!(event)
        acc

      new_state ->
        new_event = BotEvent.insert(user, user.resource, bot, loc, new_state)
        [{user, bot, new_event} | acc]
    end
  end

  defp user_state_change(true, nil, true), do: :transition_in
  defp user_state_change(true, nil, false), do: :enter

  # Debounce version
  defp user_state_change(true, be, true) do
    case be.event do
      :exit ->
        :transition_in

      :enter ->
        :no_change

      :timeout ->
        :reactivate

      :deactivate ->
        :transition_in

      :reactivate ->
        :no_change

      :transition_out ->
        :roll_back

      :transition_in ->
        debounce = Confex.get_env(:wocky, :enter_debounce_seconds)

        if debounce_expired?(be.created_at, debounce) do
          :enter
        else
          :no_change
        end
    end
  end

  # No debounce version
  defp user_state_change(true, be, false) do
    case be.event do
      :exit ->
        :enter

      :enter ->
        :no_change

      :timeout ->
        :reactivate

      :deactivate ->
        :enter

      :reactivate ->
        :no_change

      :transition_out ->
        :roll_back

      :transition_in ->
        :enter
    end
  end


  defp user_state_change(false, nil, _debounce), do: :no_change

  # Debounce version
  defp user_state_change(false, be, true) do
    case be.event do
      :exit ->
        :no_change

      :enter ->
        :transition_out

      :timeout ->
        :deactivate

      :deactivate ->
        :no_change

      :reactivate ->
        :transition_out

      :transition_in ->
        :roll_back

      :transition_out ->
        debounce = Confex.get_env(:wocky, :exit_debounce_seconds)

        if debounce_expired?(be.created_at, debounce) do
          :exit
        else
          :no_change
        end
    end
  end

  # No-debounce version
  defp user_state_change(false, be, false) do
    case be.event do
      :exit ->
        :no_change

      :enter ->
        :exit

      :timeout ->
        :deactivate

      :deactivate ->
        :no_change

      :reactivate ->
        :exit

      :transition_in ->
        :roll_back

      :transition_out ->
        :exit
    end
  end

  defp debounce_expired?(ts, wait) do
    Timex.diff(Timex.now(), ts, :seconds) >= wait
  end

  defp process_bot_event({user, bot, be}) do
    maybe_visit_bot(be.event, user, bot)
  end

  defp maybe_visit_bot(:enter, user, bot), do: Bot.visit(bot, user)

  defp maybe_visit_bot(:reactivate, user, bot), do: Bot.visit(bot, user, false)

  defp maybe_visit_bot(:exit, user, bot), do: Bot.depart(bot, user)

  defp maybe_visit_bot(:timeout, user, bot), do: Bot.depart(bot, user, false)

  defp maybe_visit_bot(_, _, _), do: :ok

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
    user = Repo.get(User, user_id)
    bot = Bot.get(bot_id)

    if user && bot do
      if Bot.subscription(bot, user) == :visitor do
        new_event = BotEvent.insert(user, resource, bot, :timeout)
        process_bot_event({user, bot, new_event})
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
