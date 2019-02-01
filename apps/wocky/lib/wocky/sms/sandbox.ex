defmodule Wocky.SMS.Sandbox do
  @moduledoc "Implements the SMS Messenger behavior in a testing sandbox"

  @behaviour Wocky.SMS.Messenger

  require Logger

  def send(recipient, body) do
    result = Confex.get_env(:wocky, :sms_send_result, :ok)

    Logger.info("""
    SMS Sandbox was requested to send "#{body}" to "#{recipient}"
    and return #{inspect(result)}
    """)

    result
  end

  def set_result(result),
    do: Application.put_env(:wocky, :sms_send_result, result)
end
