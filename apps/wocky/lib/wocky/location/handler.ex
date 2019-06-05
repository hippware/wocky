defmodule Wocky.Location.Handler do
  @moduledoc """
  This is the per-user location update handler process
  """

  use GenServer

  alias Wocky.Account
  alias Wocky.Account.User
  alias Wocky.Bot
  alias Wocky.GeoUtils
  alias Wocky.Location.BotEvent
  alias Wocky.Location.GeoFence
  alias Wocky.Location.Supervisor
  alias Wocky.Location.UserLocation
  alias Wocky.Location.UserLocation.Current
  alias Wocky.Repo

  require Logger

  @timeout :timer.hours(1)

  defmodule State do
    @moduledoc false

    defstruct [:user, :subscriptions, :events]
  end

  @spec start_link(User.t() | User.id()) :: {:ok, pid()}

  def start_link(%User{} = user), do: GenServer.start_link(__MODULE__, user)

  def start_link(user_id) do
    user_id
    |> Account.get_user()
    |> start_link()
  end

  @spec set_location(User.t() | User.id(), UserLocation.t(), boolean()) ::
          {:ok, UserLocation.t()} | {:error, any()}
  def set_location(user_or_id, location, current? \\ true) do
    user_or_id
    |> get_handler()
    |> GenServer.call({:set_location, location, current?})
  end

  @spec set_location_for_bot(User.t(), UserLocation.t(), Bot.t()) ::
          {:ok, UserLocation.t()} | {:error, any()}
  def set_location_for_bot(user, location, bot) do
    user
    |> get_handler()
    |> GenServer.call({:set_location_for_bot, location, bot})
  end

  @spec add_subscription(User.t(), Bot.t()) :: :ok
  def add_subscription(_user, %Bot{location: nil}), do: :ok

  def add_subscription(user, bot) do
    user
    |> get_handler_if_exists()
    |> maybe_call({:add_subscription, bot})
  end

  @spec remove_subscription(User.t(), Bot.t()) :: :ok
  def remove_subscription(user, bot) do
    user
    |> get_handler_if_exists()
    |> maybe_call({:remove_subscription, bot})
  end

  # Always returns a handler, creating a new one if one does not already exist.
  @spec get_handler(User.t() | User.id()) :: pid()
  def get_handler(%User{id: user_id} = user), do: do_get_handler(user_id, user)

  def get_handler(user_id) when is_binary(user_id),
    do: do_get_handler(user_id, user_id)

  defp do_get_handler(user_id, arg) do
    {:ok, pid} =
      Swarm.whereis_or_register_name(
        handler_name(user_id),
        Supervisor,
        :start_child,
        [arg],
        5000
      )

    pid
  end

  # Gets a handler if one exists, otherwise returns nil
  @spec get_handler_if_exists(User.t()) :: pid() | nil
  def get_handler_if_exists(user) do
    case Swarm.whereis_name(handler_name(user.id)) do
      :undefined -> nil
      pid -> pid
    end
  end

  defp maybe_call(nil, _), do: :ok
  defp maybe_call(pid, args), do: GenServer.call(pid, args)

  @impl true
  def init(user) do
    Logger.debug(fn -> "Swarm initializing worker with user #{user.id}" end)
    subscriptions = Account.get_subscriptions(user)
    events = BotEvent.get_last_events(user.id)

    {:ok, %State{user: user, subscriptions: subscriptions, events: events},
     @timeout}
  end

  @impl true
  def handle_call(
        {:set_location, location, current?},
        _from,
        %{user: user, subscriptions: subscriptions, events: events} = state
      ) do
    Logger.debug(fn -> "Swarm set location with user #{user.id}" end)

    with {:ok, loc} = result <- prepare_location(user, location, current?) do
      {:ok, _, new_events} =
        GeoFence.check_for_bot_events(loc, user, subscriptions, events)

      {:reply, result, %{state | events: new_events}, @timeout}
    else
      error ->
        {:reply, error, state, @timeout}
    end
  end

  def handle_call(
        {:set_location_for_bot, location, bot},
        _from,
        %{user: user, events: events} = state
      ) do
    Logger.debug(fn -> "Swarm set location for bot with user #{user.id}" end)

    with {:ok, loc} = result <- prepare_location(user, location, true) do
      {:ok, _, new_events} =
        GeoFence.check_for_bot_event(bot, loc, user, events)

      {:reply, result, %{state | events: new_events}, @timeout}
    else
      error ->
        {:reply, error, state, @timeout}
    end
  end

  def handle_call({:add_subscription, bot}, _from, state) do
    subs = do_remove_subscription(bot, state.subscriptions)
    {:reply, :ok, %{state | subscriptions: [bot | subs]}}
  end

  def handle_call({:remove_subscription, bot}, _from, state) do
    subs = do_remove_subscription(bot, state.subscriptions)
    {:reply, :ok, %{state | subscriptions: subs}}
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

  # called when a network split is healed and the local process
  # should continue running, but a duplicate process on the other
  # side of the split is handing off its state to us. You can choose
  # to ignore the handoff state, or apply your own conflict resolution
  # strategy
  @impl true
  def handle_cast({:swarm, :resolve_conflict, _state}, state) do
    {:noreply, state}
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

  defp handler_name(user_id), do: "location_handler_" <> user_id

  defp do_remove_subscription(bot, subscriptions) do
    Enum.reject(subscriptions, fn b -> b.id == bot.id end)
  end

  defp prepare_location(user, location, current?) do
    with nloc <- normalize_location(location),
         {:ok, loc} <- maybe_save_location(user, nloc),
         :ok <- maybe_save_current_location(current?, user, nloc) do
      {:ok, loc}
    end
  end

  defp normalize_location(location) do
    {nlat, nlon} = GeoUtils.normalize_lat_lon(location.lat, location.lon)

    %UserLocation{
      location
      | lat: nlat,
        lon: nlon,
        captured_at: normalize_captured_at(location),
        created_at: DateTime.utc_now()
    }
  end

  defp normalize_captured_at(%UserLocation{captured_at: time})
       when is_binary(time) do
    {:ok, dt, 0} = DateTime.from_iso8601(time)
    dt
  end

  defp normalize_captured_at(%UserLocation{captured_at: %DateTime{} = dt}),
    do: dt

  defp normalize_captured_at(_), do: DateTime.utc_now()

  defp maybe_save_location(user, location) do
    if should_save_location?(user) do
      save_location(user, location)
    else
      {:ok, location}
    end
  end

  defp should_save_location?(user) do
    GeoFence.save_locations?() || Account.hippware?(user)
  end

  def save_location(user, location) do
    %UserLocation{user_id: user.id}
    |> UserLocation.changeset(Map.from_struct(location))
    |> Repo.insert()
  end

  defp maybe_save_current_location(false, _user, _location), do: :ok

  defp maybe_save_current_location(true, user, location) do
    if GeoFence.should_process?(location, GeoFence.get_config()) do
      Current.set(user, location)
    else
      :ok
    end
  end
end
