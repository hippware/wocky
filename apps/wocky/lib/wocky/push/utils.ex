defmodule Wocky.Push.Utils do
  @moduledoc """
  Utility functions to help with push notifications
  """

  @message_limit 512

  def maybe_truncate_message(message) do
    if byte_size(message) > @message_limit do
      String.slice(message, 0, @message_limit - 3) <> "..."
    else
      message
    end
  end

  def timeout, do: get_conf(:timeout)

  def sandbox?, do: get_conf(:sandbox)

  def enabled?, do: get_conf(:enabled)

  def log_payload?, do: get_conf(:log_payload)

  defp get_conf(key), do: Confex.get_env(:wocky, Wocky.Push)[key]
end
