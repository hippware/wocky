defmodule Wocky.Location do
  @moduledoc "Bounded context for location aware code"

  import Ecto.Query

  alias Ecto.{Changeset, Queryable}
  alias Wocky.Account.User
  alias Wocky.Bot
  alias Wocky.Repo

  alias Wocky.Location.{
    BotEvent,
    GeoFence,
    Handler,
    Share,
    Share.Cache,
    UserLocation,
    UserLocation.Current
  }

  # ----------------------------------------------------------------------
  # Subscription Cache Management

  defdelegate add_subscription(user, bot), to: Handler

  defdelegate remove_subscription(user, bot), to: Handler

  # ----------------------------------------------------------------------
  # User Location

  @spec get_user_locations_query(User.t(), User.device()) :: Queryable.t()
  def get_user_locations_query(%User{id: user_id}, device) do
    UserLocation
    |> where(user_id: ^user_id)
    |> where(device: ^device)
  end

  @spec get_user_location_events_query(
          User.t(),
          User.device() | UserLocation.t()
        ) ::
          Queryable.t()
  def get_user_location_events_query(_user, %UserLocation{} = loc) do
    Ecto.assoc(loc, :events)
  end

  def get_user_location_events_query(%User{id: user_id}, device)
      when is_binary(device) do
    BotEvent
    |> where(user_id: ^user_id)
    |> where(device: ^device)
  end

  @spec set_user_location(User.t(), User.device(), float(), float(), float()) ::
          :ok | {:error, any}
  def set_user_location(user, device, lat, lon, accuracy) do
    location = %UserLocation{
      lat: lat,
      lon: lon,
      accuracy: accuracy,
      device: device
    }

    with {:ok, _} <- set_user_location(user, location) do
      :ok
    end
  end

  @doc """
  Sets the user's current location to the provided Location struct and runs the
  geofence calculation for all of the user's subscribed bots.
  """
  @spec set_user_location(User.t(), UserLocation.t(), boolean()) ::
          {:ok, UserLocation.t()} | {:error, any}
  defdelegate set_user_location(user, location, current? \\ true),
    to: Handler,
    as: :set_location

  @doc """
  Sets the user's current location to the provided Location struct and runs the
  geofence calculation for the specified bot only and with debouncing disabled.
  """
  @spec set_user_location_for_bot(User.t(), UserLocation.t(), Bot.t()) ::
          {:ok, UserLocation.t()} | {:error, any}
  defdelegate set_user_location_for_bot(user, location, bot),
    to: Handler,
    as: :set_location_for_bot

  @doc "Gets the current location for the user."
  @spec get_current_user_location(User.t()) :: UserLocation.t() | nil
  defdelegate get_current_user_location(user), to: Current, as: :get

  @doc "Cleans up current location cache."
  @spec clean_current_locations([User.t()]) :: :ok
  defdelegate clean_current_locations(user_ids),
    to: Current,
    as: :delete_when_not_shared

  defdelegate exit_bot(user, bot, reason), to: GeoFence

  # ----------------------------------------------------------------------
  # Live Location Sharing

  @spec start_sharing_location(User.t(), User.t(), DateTime.t()) ::
          {:ok, Share.t()} | {:error, Changeset.t() | atom}
  def start_sharing_location(user, shared_with, expiry) do
    result =
      %Share{}
      |> Share.changeset(%{
        user_id: user.id,
        shared_with_id: shared_with.id,
        expires_at: expiry
      })
      |> Repo.insert(
        on_conflict: [set: [expires_at: expiry, updated_at: DateTime.utc_now()]],
        conflict_target: [:user_id, :shared_with_id]
      )

    result
  end

  @spec stop_sharing_location(User.t(), User.t()) :: :ok
  def stop_sharing_location(%User{id: user_id}, %User{id: shared_with_id}) do
    Share
    |> where(user_id: ^user_id, shared_with_id: ^shared_with_id)
    |> Repo.delete_all()

    :ok
  end

  @spec stop_sharing_location(User.t()) :: :ok
  def stop_sharing_location(%User{id: user_id}) do
    Share
    |> where(user_id: ^user_id)
    |> Repo.delete_all()

    :ok
  end

  @spec get_location_share_targets(User.t()) :: [User.id()]
  def get_location_share_targets(user), do: Cache.get(user.id)

  @spec get_location_shares(User.t()) :: [Share.t()]
  def get_location_shares(user) do
    user
    |> get_location_shares_query()
    |> Repo.all()
  end

  @spec get_location_shares_query(User.t()) :: Queryable.t()
  def get_location_shares_query(%User{id: user_id}) do
    location_shares_query()
    |> where([ls], ls.user_id == ^user_id)
  end

  @spec get_location_sharers(User.t()) :: [Share.t()]
  def get_location_sharers(user) do
    user
    |> get_location_sharers_query()
    |> Repo.all()
  end

  @spec get_location_sharers_query(User.t()) :: Queryable.t()
  def get_location_sharers_query(%User{id: user_id}) do
    location_shares_query()
    |> where([ls], ls.shared_with_id == ^user_id)
  end

  defp location_shares_query do
    now = DateTime.utc_now()

    Share
    |> preload([:user, :shared_with])
    |> where([ls], ls.expires_at > ^now)
    |> order_by([ls], desc: ls.created_at)
  end

  @spec clean_expired_shares() :: {non_neg_integer(), [User.id()]}
  def clean_expired_shares do
    {count, user_ids} =
      Share
      |> select([l], l.user_id)
      |> where([l], l.expires_at <= ^DateTime.utc_now())
      |> Repo.delete_all(timeout: :infinity)

    {count, Enum.uniq(user_ids)}
  end

  @spec refresh_share_cache(User.id()) :: [User.id()]
  def refresh_share_cache(user_id), do: Cache.refresh(user_id)
end
