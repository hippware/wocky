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
  # Audit management

  @doc "Enable auditing for a user"
  @spec enable_user_audit(User.t()) :: :ok
  def enable_user_audit(user) do
    {:ok, true} = FunWithFlags.enable(:traffic, for_actor: user)
    {:ok, true} = FunWithFlags.enable(:location, for_actor: user)
    {:ok, true} = FunWithFlags.enable(:push, for_actor: user)
    {:ok, true} = FunWithFlags.enable(:push_payload, for_actor: user)

    :ok
  end

  @doc "Enable auditing for a user"
  @spec disable_user_audit(User.t()) :: :ok
  def disable_user_audit(user) do
    {:ok, false} = FunWithFlags.disable(:traffic, for_actor: user)
    {:ok, false} = FunWithFlags.disable(:location, for_actor: user)
    {:ok, false} = FunWithFlags.disable(:push, for_actor: user)
    {:ok, false} = FunWithFlags.disable(:push_payload, for_actor: user)

    :ok
  end

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
          {:ok, String.t() | nil}
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
    |> Map.put(
      :payload_string,
      maybe_extract_payload(fields.payload_string, user, config)
    )
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
