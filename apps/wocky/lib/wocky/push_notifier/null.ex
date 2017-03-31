defmodule Wocky.PushNotifier.Null do
  @moduledoc """
  Implements a null `PushNotifier`. All operations are no-ops that ignore
  their inputs. This handler effectively disables push notifications.
  """

  require Logger

  @behaviour Wocky.PushNotifier

  @spec init :: :ok
  def init do
    Logger.info("Initialising null Wocky push notifier")
  end

  @spec register(binary, binary, binary, binary) :: {:ok, binary}
  def register(user, resource, platform, device) do
    :ok = Logger.info("""
      Push notification registration request for #{user}'s device '#{device}' \
      for resource '#{resource}' on the '#{platform}' platform\
      """)
    {:ok, device}
  end

  @spec push(binary, binary) :: :ok
  def push(endpoint, message) do
    :ok = Logger.info("""
      Push notification to '#{endpoint}' with message: #{message}
      """)
    :ok
  end
end
