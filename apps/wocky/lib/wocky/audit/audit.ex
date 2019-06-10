defmodule Wocky.Audit do
  @moduledoc "Context for user audit logging"

  use Wocky.Config

  import Ecto.Query

  alias Ecto.Changeset
  alias Ecto.Queryable
  alias Timex.Duration
  alias Wocky.Account
  alias Wocky.Account.User
  alias Wocky.Audit.LocationLog
  alias Wocky.Audit.PushLog
  alias Wocky.Audit.TrafficLog
  alias Wocky.Location.BotEvent
  alias Wocky.Repo

  # ===================================================================
  # Traffic logging

  @doc "Write a packet record to the database"
  @spec log_traffic(map(), User.t(), Keyword.t()) ::
          {:ok, TrafficLog.t() | nil} | {:error, Changeset.t()}
  def log_traffic(fields, user, opts \\ []) do
    config = config(opts)

    if should_log?(:traffic, user, config) do
      fields
      |> TrafficLog.changeset()
      |> Repo.insert()
    else
      {:ok, nil}
    end
  end

  @spec get_traffic_by_period(User.id(), DateTime.t(), Duration.t()) ::
          [TrafficLog.t()]
  def get_traffic_by_period(user_id, start, duration) do
    TrafficLog
    |> users_traffic(user_id, start, duration)
    |> Repo.all()
  end

  @spec get_traffic_by_device(User.id(), binary, DateTime.t(), Duration.t()) ::
          [TrafficLog.t()]
  def get_traffic_by_device(user_id, device, start, duration) do
    TrafficLog
    |> device_traffic(user_id, device, start, duration)
    |> Repo.all()
  end

  defp users_traffic(query, user_id, startt, duration) do
    endt = Timex.add(startt, duration)

    from t in query,
      where: t.user_id == ^user_id,
      where: t.created_at >= ^startt,
      where: t.created_at <= ^endt,
      select: t,
      order_by: [asc: :created_at]
  end

  defp device_traffic(query, user_id, device, startt, duration) do
    query = users_traffic(query, user_id, startt, duration)
    from t in query, where: t.device == ^device
  end

  # ===================================================================
  # Location logging

  @doc "Write a location record to the database"
  @spec log_location(map(), User.t(), Keyword.t()) ::
          {:ok, LocationLog.t() | nil} | {:error, Changeset.t()}
  def log_location(fields, user, opts \\ []) do
    config = config(opts)

    if should_log?(:location, user, config) do
      %LocationLog{user_id: user.id}
      |> LocationLog.changeset(fields)
      |> Repo.insert()
    else
      {:ok, nil}
    end
  end

  @spec get_locations_query(User.t(), User.device()) :: Queryable.t()
  def get_locations_query(%User{id: user_id}, device) do
    LocationLog
    |> where(user_id: ^user_id)
    |> where(device: ^device)
  end

  @spec get_location_events_query(
          User.t(),
          User.device() | LocationLog.t()
        ) ::
          Queryable.t()
  def get_location_events_query(_user, %LocationLog{} = loc) do
    Ecto.assoc(loc, :events)
  end

  def get_location_events_query(%User{id: user_id}, device)
      when is_binary(device) do
    BotEvent
    |> where(user_id: ^user_id)
    |> where(device: ^device)
  end

  # ===================================================================
  # Push notification logging

  @doc "Write a push notification record to the database"
  @spec log_push(map(), User.t(), Keyword.t()) ::
          {:ok, PushLog.t() | nil} | {:error, Changeset.t()}
  def log_push(fields, user, opts \\ []) do
    config = config(opts)

    if should_log?(:push, user, config) do
      fields
      |> format_msg(user, config)
      |> PushLog.insert_changeset()
      |> Repo.insert()
    else
      {:ok, nil}
    end
  end

  defp format_msg(fields, user, config) do
    fields
    |> Map.take([:device, :token, :message_id, :response, :details])
    |> Map.put(:user_id, user.id)
    |> Map.put(:payload, maybe_extract_payload(fields.payload, user, config))
  end

  defp maybe_extract_payload(payload, user, config) do
    if should_log?(:push_payload, user, config) do
      inspect(payload)
    end
  end

  # ===================================================================
  # Helpers

  defp should_log?(mode, user, config) do
    (user && Account.hippware?(user)) || Map.get(config, config_key(mode))
  end

  defp config_key(mode), do: String.to_existing_atom("log_#{mode}")
end
