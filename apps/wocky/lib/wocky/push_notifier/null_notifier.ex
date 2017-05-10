defmodule Wocky.PushNotifier.NullNotifier do
  @moduledoc """
  Implements a null `PushNotifier`. All operations are no-ops that ignore
  their inputs. This handler effectively disables notifications.
  """

  require Logger

  @behaviour Wocky.PushNotifier

  def init do
    Logger.info("Notifications disabled")
  end

  def enable(_user, _server, _resource, _platform, device) do
    {:ok, Base.encode64(device)}
  end

  def disable(_endpoint) do
    :ok
  end

  def push(_body, _endpoint) do
    :ok
  end
end
