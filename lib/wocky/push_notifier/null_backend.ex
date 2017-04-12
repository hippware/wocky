defmodule Wocky.PushNotifier.NullBackend do
  @moduledoc """
  Implements a null `PushNotifier`. All operations are no-ops that ignore
  their inputs. This handler effectively disables notifications.
  """

  require Logger

  @behaviour Wocky.PushNotifier

  def init do
    Logger.info("""
    Initialising null Wocky push notification handler
    """)
  end

  def enable(user, _server, _resource, platform, device) do
    :ok = Logger.info("""
      Notification registration request for #{user}'s device '#{device}' \
      on the '#{platform}' platform\
      """)
    {:ok, device}
  end

  def disable(endpoint) do
    Logger.info("""
    Disabling endpoint '#{endpoint}'\
    """)
  end

  def push(body, endpoint) do
    Logger.info("""
    Notification to '#{endpoint}' with body: #{body}\
    """)
  end
end
