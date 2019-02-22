defmodule Wocky.User.Location.Handler do
  @moduledoc """
  This is the per-user location update handler process
  """

  alias Wocky.{GeoUtils, Location, Repo, User}
  alias Wocky.User.{GeoFence, Location}

  use GenServer

  @spec start_link(User.t()) :: {:ok, pid()}
  def start_link(user), do: GenServer.start_link(__MODULE__, user)

  def get_handler(user) do
    {:ok, pid} =
      Swarm.whereis_or_register_name(
        handler_name(user),
        __MODULE__,
        :start_link,
        [user],
        5000
      )

    pid
  end

  def init(user), do: {:ok, user}

  # called when a handoff has been initiated due to changes
  # in cluster topology, valid response values are:
  #
  #   - `:restart`, to simply restart the process on the new node
  #   - `{:resume, state}`, to hand off some state to the new process
  #   - `:ignore`, to leave the process running on its current node
  #
  def handle_call({:swarm, :begin_handoff}, _from, user),
    do: {:reply, {:resume, user}, user}

  # called after the process has been restarted on its new node,
  # and the old process' state is being handed off. This is only
  # sent if the return to `begin_handoff` was `{:resume, state}`.
  def handle_cast({:swarm, :end_handoff, _user}, user),
    do: {:noreply, user}

  def handle_call({:set_location, location}, _from, user) do
    reply =
      with {:ok, loc} = result <- prepare_location(user, location) do
        GeoFence.check_for_bot_events(loc, user)
        result
      end

    {:reply, reply, user}
  end

  def handle_call({:set_location_for_bot, location, bot}, _from, user) do
    reply =
      with {:ok, loc} = result <- prepare_location(user, location) do
        GeoFence.check_for_bot_event(bot, loc, user)
        result
      end

    {:reply, reply, user}
  end

  # called when a network split is healed and the local process
  # should continue running, but a duplicate process on the other
  # side of the split is handing off its state to us. You can choose
  # to ignore the handoff state, or apply your own conflict resolution
  # strategy
  def handle_cast({:swarm, :resolve_conflict, _state}, state),
    do: {:noreply, state}

  # this message is sent when this process should die
  # because it is being moved, use this as an opportunity
  # to clean up
  def handle_info({:swarm, :die}, state), do: {:stop, :shutdown, state}

  defp handler_name(user), do: "location_handler_" <> user.id

  defp prepare_location(user, location) do
    with nloc <- normalize_location(location),
         {:ok, loc} <- maybe_save_location(user, nloc),
         {:ok, _} <- save_current_location(user, nloc) do
      {:ok, loc}
    end
  end

  defp normalize_location(location) do
    {nlat, nlon} = GeoUtils.normalize_lat_lon(location.lat, location.lon)
    captured_at = normalize_captured_at(location)
    %Location{location | lat: nlat, lon: nlon, captured_at: captured_at}
  end

  defp normalize_captured_at(%Location{captured_at: time})
       when not is_nil(time),
       do: time

  defp normalize_captured_at(_), do: DateTime.utc_now()

  defp maybe_save_location(user, location) do
    if should_save_location?(user) do
      save_location(user, location)
    else
      {:ok, location}
    end
  end

  defp should_save_location?(user) do
    GeoFence.save_locations?() || User.hippware?(user)
  end

  def save_location(user, location) do
    user
    |> Ecto.build_assoc(:locations)
    |> Location.changeset(Map.from_struct(location))
    |> Repo.insert()
  end

  defp save_current_location(user, location) do
    user
    |> Ecto.build_assoc(:current_location)
    |> Location.changeset(Map.from_struct(location))
    |> Repo.insert(
      on_conflict: {:replace, Location.fields() ++ [:updated_at]},
      conflict_target: :user_id
    )
  end
end
