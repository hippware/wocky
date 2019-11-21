defmodule Wocky.Location do
  @moduledoc "Bounded context for location aware code"

  alias Geocalc.Point
  alias Wocky.Account.User
  alias Wocky.Location.GeoFence
  alias Wocky.Location.Handler
  alias Wocky.Location.UserLocation
  alias Wocky.Location.UserLocation.Current
  alias Wocky.POI.Bot

  # ----------------------------------------------------------------------
  # Subscription Cache Management

  @spec refresh_bot_subscriptions(User.tid()) :: :ok
  defdelegate refresh_bot_subscriptions(user), to: Handler

  @spec refresh_proximity_subscriptions(User.tid()) :: :ok
  defdelegate refresh_proximity_subscriptions(user), to: Handler

  # ----------------------------------------------------------------------
  # User Location

  @doc """
  Sets the user's current location to the provided Location struct and runs the
  geofence calculation for all of the user's subscribed bots.
  """
  @spec set_user_location(User.tid(), UserLocation.t(), boolean()) ::
          {:ok, UserLocation.t()} | {:error, any}
  defdelegate set_user_location(user, location, current? \\ true),
    to: Handler,
    as: :set_location

  @doc """
  Sets the user's current location to the provided Location struct and runs the
  geofence calculation for the specified bot only and with debouncing disabled.
  """
  @spec set_user_location_for_bot(User.tid(), UserLocation.t(), Bot.t()) ::
          {:ok, UserLocation.t()} | {:error, any}
  defdelegate set_user_location_for_bot(user, location, bot),
    to: Handler,
    as: :set_location_for_bot

  @doc "Gets the current location for the user."
  @spec get_current_user_location(User.tid()) :: UserLocation.t() | nil
  defdelegate get_current_user_location(user), to: Current, as: :get

  @doc "Cleans up current location cache."
  @spec clean_current_locations([User.id()]) :: non_neg_integer()
  defdelegate clean_current_locations(user_ids),
    to: Current,
    as: :delete_when_not_shared

  @spec exit_bot(User.tid(), Bot.t(), String.t()) :: :ok
  defdelegate exit_bot(user, bot, reason), to: GeoFence

  @spec inc_watcher_count(User.tid()) :: :ok
  defdelegate inc_watcher_count(user), to: Handler

  @spec dec_watcher_count(User.tid()) :: :ok
  defdelegate dec_watcher_count(user), to: Handler

  @spec get_watched_status(User.tid()) :: Handler.watched_status()
  defdelegate get_watched_status(user), to: Handler

  @spec to_point(UserLocation.t()) :: Point.t()
  def to_point(location), do: Map.take(location, [:lat, :lon])
end
