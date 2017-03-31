defmodule Wocky.Notification.NullHandler do
  @moduledoc """
  Implements a null `wocky_notification_handler`. All operations are no-ops
  that ignore their inputs. This handler effectively disables notifications.
  """

  require Logger

  # FIXME @behaviour :wocky_notification_handler

  @spec init :: :ok
  def init do
    :ok = Logger.info("""
      Initialising null Wocky notification handler
      """)
    :ok
  end

  @spec register(binary, binary, binary) :: {:ok, binary}
  def register(user, platform, device) do
    :ok = Logger.info("""
      Notification registration request for #{user}'s device '#{device}' \
      on the '#{platform}' platform\
      """)
    {:ok, device}
  end

  @spec notify_message(binary, binary, binary) :: :ok
  def notify_message(endpoint, from, body) do
    :ok = Logger.info("""
      Message notification to '#{endpoint}' from #{from} with body: #{body}\
      """)
    :ok
  end

  @spec notify(binary, binary) :: :ok
  def notify(endpoint, message) do
    :ok = Logger.info("""
      Generic notification to '#{endpoint}' with message: #{message}\
      """)
    :ok
  end
end
