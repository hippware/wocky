defmodule Wocky.Notification.NullHandler do
  @moduledoc """
  Implements a null `wocky_notification_handler`. All operations are no-ops
  that ignore their inputs. This handler effectively disables notifications.
  """

  require Logger

  @behaviour :wocky_notification_handler

  def register(user, platform, device) do
    :ok = Logger.info("""
      Notification registration request for #{user}'s device '#{device}'
      on the '#{platform}' platform
      """)
    {:ok, device}
  end

  def notify(endpoint, from, body) do
    :ok = Logger.info("""
      Notification request to '#{endpoint}' from #{from} with body: #{body}
      """)
    :ok
  end
end
