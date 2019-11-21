defmodule Wocky.Location.Handler do
  @moduledoc """
  This is the per-user location update handler process
  """

  use GenServer, restart: :temporary

  alias Wocky.Account.User
  alias Wocky.Audit
  alias Wocky.Location.BotEvent
  alias Wocky.Location.GeoFence
  alias Wocky.Location.Supervisor
  alias Wocky.Location.UserLocation
  alias Wocky.Location.UserLocation.Current
  alias Wocky.Location.UserProximity
  alias Wocky.Location.UserProximity.Subscription, as: ProximitySubscription
  alias Wocky.POI.Bot
  alias Wocky.Relation

  require Logger

  @swarm_group :location_handlers
  @timeout :timer.hours(1)
  @watcher_debounce_secs 5 * 60

  defmodule State do
    @moduledoc false

    @type t :: %__MODULE__{
            user: User.t(),
            events: BotEvent.bot_event_map(),
            watcher_count: non_neg_integer(),
            watched_status_changed_at: DateTime.t() | nil,
            bot_subscriptions: [Bot.t()],
            proximity_subscriptions: [ProximitySubscription.t()],
            proximity_subscribers: [ProximitySubscription.t()]
          }

    defstruct user: nil,
              events: %{},
              watcher_count: 0,
              watched_status_changed_at: nil,
              bot_subscriptions: [],
              proximity_subscriptions: [],
              proximity_subscribers: []
  end

  @type watched_status :: %{
          watched: boolean(),
          watchers: non_neg_integer(),
          changed_at: DateTime.t() | nil
        }

  @spec start_link(User.tid()) :: {:ok, pid()}
  def start_link(user) do
    GenServer.start_link(__MODULE__, User.hydrate(user))
  end

  @spec set_location(User.tid(), UserLocation.t(), boolean()) ::
          {:ok, UserLocation.t()} | {:error, any()}
  def set_location(user, location, current? \\ true) do
    user
    |> get_handler()
    |> GenServer.call({:set_location, location, current?})
  end

  @spec set_location_for_bot(User.tid(), UserLocation.t(), Bot.t()) ::
          {:ok, UserLocation.t()} | {:error, any()}
  def set_location_for_bot(user, location, bot) do
    user
    |> get_handler()
    |> GenServer.call({:set_location_for_bot, location, bot})
  end

  @spec refresh_bot_subscriptions(User.tid()) :: :ok
  def refresh_bot_subscriptions(user) do
    user
    |> get_handler_if_exists()
    |> maybe_call(:refresh_bot_subscriptions)
  end

  @spec set_proximity_location(User.tid(), User.t(), UserLocation.t()) :: :ok
  def set_proximity_location(user, source_user, location) do
    user
    |> get_handler()
    |> GenServer.cast({:set_proximity_location, source_user, location})
  end

  @spec refresh_proximity_subscriptions(User.tid()) :: :ok
  def refresh_proximity_subscriptions(user) do
    user
    |> get_handler_if_exists()
    |> maybe_call(:refresh_proximity_subscriptions)
  end

  @spec inc_watcher_count(User.tid()) :: :ok
  def inc_watcher_count(user) do
    user
    |> get_handler()
    |> GenServer.cast(:inc_watcher_count)
  end

  @spec dec_watcher_count(User.tid()) :: :ok
  def dec_watcher_count(user) do
    user
    |> get_handler()
    |> GenServer.cast(:dec_watcher_count)
  end

  @spec get_watched_status(User.tid()) :: watched_status()
  def get_watched_status(user) do
    user
    |> get_handler()
    |> GenServer.call(:get_watched_status)
  end

  # Shutdown an existing handler, if present. Used only in testing to release
  # DB checkouts
  @spec stop(User.tid()) :: :ok
  def stop(user) do
    user
    |> get_handler_if_exists()
    |> maybe_stop()
  end

  @spec stop_all :: :ok
  def stop_all do
    @swarm_group
    |> Swarm.members()
    |> Enum.each(&maybe_stop/1)
  end

  @doc """
  Always returns a handler, creating a new one if one does not already exist.
  """
  @spec get_handler(User.tid()) :: pid()
  def get_handler(user) do
    {:ok, pid} =
      Swarm.whereis_or_register_name(
        handler_name(user),
        Supervisor,
        :start_child,
        [user],
        5000
      )

    Swarm.join(@swarm_group, pid)

    pid
  end

  @doc "Gets a handler if one exists, otherwise returns nil."
  @spec get_handler_if_exists(User.tid()) :: pid() | nil
  def get_handler_if_exists(user) do
    case Swarm.whereis_name(handler_name(user)) do
      :undefined -> nil
      pid -> pid
    end
  end

  defp maybe_call(nil, _), do: :ok
  defp maybe_call(pid, args), do: GenServer.call(pid, args)

  defp maybe_stop(nil), do: :ok
  defp maybe_stop(pid), do: GenServer.stop(pid, :normal)

  @impl true
  def init(user) do
    Logger.debug(fn -> "Swarm initializing worker with user #{user.id}" end)
    bot_subscriptions = Relation.get_subscribed_bots(user)
    events = BotEvent.get_last_events(user.id)
    proximity_subscriptions = UserProximity.get_subscriptions(user)
    proximity_subscribers = UserProximity.get_subscribers(user)

    {:ok,
     %State{
       user: user,
       events: events,
       bot_subscriptions: bot_subscriptions,
       proximity_subscriptions: proximity_subscriptions,
       proximity_subscribers: proximity_subscribers
     }, @timeout}
  end

  @impl true
  def handle_call(
        {:set_location, location, current?},
        _from,
        %{
          user: user,
          bot_subscriptions: bot_subscriptions,
          events: events,
          proximity_subscriptions: proximity_subscriptions,
          proximity_subscribers: proximity_subscribers
        } = state
      ) do
    Logger.debug(fn -> "Swarm set location with user #{user.id}" end)

    case prepare_location(user, location, current?) do
      {:ok, loc} = result ->
        proximity_subscriptions =
          UserProximity.check_targets(user, location, proximity_subscriptions)

        UserProximity.notify_subscribers(user, location, proximity_subscribers)

        {:ok, _, new_events} =
          GeoFence.check_for_bot_events(loc, user, bot_subscriptions, events)

        {:reply, result,
         %{
           state
           | events: new_events,
             proximity_subscriptions: proximity_subscriptions
         }, @timeout}

      {:error, _} = error ->
        {:reply, error, state, @timeout}
    end
  end

  def handle_call(
        {:set_location_for_bot, location, bot},
        _from,
        %{user: user, events: events} = state
      ) do
    Logger.debug(fn -> "Swarm set location for bot with user #{user.id}" end)

    case prepare_location(user, location, true) do
      {:ok, loc} = result ->
        {:ok, _, new_events} =
          GeoFence.check_for_bot_event(bot, loc, user, events)

        {:reply, result, %{state | events: new_events}, @timeout}

      {:error, _} = error ->
        {:reply, error, state, @timeout}
    end
  end

  def handle_call(:refresh_bot_subscriptions, _from, state) do
    bot_subscriptions = Relation.get_subscribed_bots(state.user)
    {:reply, :ok, %{state | bot_subscriptions: bot_subscriptions}, @timeout}
  end

  def handle_call(:refresh_proximity_subscriptions, _from, state) do
    proximity_subscriptions = UserProximity.get_subscriptions(state.user)
    proximity_subscribers = UserProximity.get_subscribers(state.user)

    {:reply, :ok,
     %{
       state
       | proximity_subscribers: proximity_subscribers,
         proximity_subscriptions: proximity_subscriptions
     }, @timeout}
  end

  def handle_call(
        :get_watched_status,
        _from,
        %{watched_status_changed_at: changed_at, watcher_count: count} = state
      ) do
    watched = !watchers_expired?(count, changed_at)
    result = %{watchers: count, changed_at: changed_at, watched: watched}

    {:reply, result, state, @timeout}
  end

  # called when a handoff has been initiated due to changes
  # in cluster topology, valid response values are:
  #
  #   - `:restart`, to simply restart the process on the new node
  #   - `{:resume, state}`, to hand off some state to the new process
  #   - `:ignore`, to leave the process running on its current node
  #
  def handle_call({:swarm, :begin_handoff}, _from, state) do
    Logger.debug(fn -> "Swarm handing off state with user #{state.user.id}" end)
    {:reply, :restart, state}
  end

  @impl true
  def handle_cast(
        {:set_proximity_location, %User{id: source_id}, source_location},
        %{user: user, proximity_subscriptions: proximity_subscriptions} = state
      ) do
    proximity_subscriptions =
      proximity_subscriptions
      |> Enum.map(fn
        %{target_id: ^source_id} = s ->
          UserProximity.check_for_notify(user, source_location, s)

        sub ->
          sub
      end)

    {:noreply, %{state | proximity_subscriptions: proximity_subscriptions},
     @timeout}
  end

  def handle_cast(:inc_watcher_count, %{watcher_count: 0} = state) do
    {:noreply,
     %{state | watcher_count: 1, watched_status_changed_at: DateTime.utc_now()},
     @timeout}
  end

  def handle_cast(:inc_watcher_count, %{watcher_count: count} = state) do
    {:noreply, %{state | watcher_count: count + 1}, @timeout}
  end

  def handle_cast(:dec_watcher_count, %{watcher_count: 0} = state) do
    {:noreply, state, @timeout}
  end

  def handle_cast(:dec_watcher_count, %{watcher_count: count} = state) do
    new_count = count - 1

    changed_at =
      if new_count == 0 do
        DateTime.utc_now()
      else
        state.watched_status_changed_at
      end

    {:noreply,
     %{state | watcher_count: new_count, watched_status_changed_at: changed_at},
     @timeout}
  end

  # called when a network split is healed and the local process
  # should continue running, but a duplicate process on the other
  # side of the split is handing off its state to us. You can choose
  # to ignore the handoff state, or apply your own conflict resolution
  # strategy
  def handle_cast({:swarm, :resolve_conflict, _state}, state) do
    {:noreply, state, @timeout}
  end

  # this message is sent when this process should die
  # because it is being moved, use this as an opportunity
  # to clean up
  @impl true
  def handle_info({:swarm, :die}, state), do: {:stop, :shutdown, state}

  def handle_info(:timeout, %{user: user} = state) do
    Logger.debug(fn -> "Swarm worker for user #{user.id} idle timeout" end)
    {:stop, :shutdown, state}
  end

  defp watchers_expired?(0, changed_at) do
    Timex.diff(Timex.now(), changed_at, :seconds) >= @watcher_debounce_secs
  end

  defp watchers_expired?(_count, _changed_at), do: false

  defp handler_name(user), do: "location_handler_" <> User.id(user)

  defp prepare_location(user, location, current?) do
    with :ok <- UserLocation.validate(location) do
      nloc =
        case Audit.log_location(location, user) do
          {:ok, log_id} when not is_nil(log_id) ->
            %UserLocation{location | id: log_id}

          _else ->
            location
        end

      maybe_save_current_location(current?, user, nloc)

      {:ok, nloc}
    end
  end

  defp maybe_save_current_location(false, _user, _location), do: :ok

  defp maybe_save_current_location(true, user, location) do
    if GeoFence.should_process?(location, GeoFence.config()) do
      Current.set(user, location)
    else
      :ok
    end
  end
end
