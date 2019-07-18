defmodule Wocky.Audit do
  @moduledoc "Context for user audit logging"

  use ModuleConfig, otp_app: :wocky

  require Logger

  alias Wocky.Account.User
  alias Wocky.Audit.PushLog
  alias Wocky.Audit.TrafficLog
  alias Wocky.Location.UserLocation
  alias Wocky.Repo.ID

  # ===================================================================
  # Traffic logging

  @doc "Write a packet record to the database"
  @spec log_traffic(TrafficLog.t(), User.t(), Keyword.t()) :: :ok
  def log_traffic(fields, user, opts \\ []) do
    config = config(opts)
    maybe_log(fields, :traffic, user, config)
  end

  # ===================================================================
  # Location logging

  @doc "Write a location record to the database"
  @spec log_location(UserLocation.t(), User.t(), Keyword.t()) ::
          {:ok, binary() | nil}
  def log_location(location, user, opts \\ []) do
    config = config(opts)

    if should_log?(:location, user, config) do
      id = ID.new()
      location = %UserLocation{location | id: id, user_id: user.id}

      _ = do_log(location, :location)

      {:ok, id}
    else
      {:ok, nil}
    end
  end

  # ===================================================================
  # Push notification logging

  @doc "Write a push notification record to the database"
  @spec log_push(PushLog.t(), User.t(), Keyword.t()) :: :ok
  def log_push(fields, user, opts \\ []) do
    config = config(opts)

    fields
    |> format_msg(user, config)
    |> maybe_log(:push, user, config)
  end

  defp format_msg(fields, user, config) do
    fields
    |> Map.take([:device, :token, :message_id, :response, :details])
    |> Map.put(:user_id, user.id)
    |> Map.put(:payload, maybe_extract_payload(fields.payload, user, config))
  end

  defp maybe_extract_payload(payload, user, config) do
    if should_log?(:push_payload, user, config) do
      payload
    end
  end

  # ===================================================================
  # Helpers

  defp maybe_log(fields, type, user, config) do
    if should_log?(type, user, config) do
      do_log(fields, type)
    else
      :ok
    end
  end

  defp do_log(fields, type) do
    Logger.info(Poison.encode!(fields), class: :audit, audit_type: type)
  end

  defp should_log?(mode, user, config) do
    FunWithFlags.Group.in?(user, :hippware) ||
      FunWithFlags.enabled?(mode, for: user) ||
      Map.get(config, config_key(mode))
  end

  defp config_key(mode), do: String.to_existing_atom("log_#{mode}")
end
