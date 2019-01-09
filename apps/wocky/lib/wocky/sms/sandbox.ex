defmodule Wocky.SMS.Sandbox do
  @moduledoc "Implements the SMS Messenger behavior in a testing sandbox"

  @behaviour Wocky.SMS.Messenger

  def send(_recipient, _body) do
    Confex.get_env(:wocky, :sms_send_result, :ok)
  end

  def set_result(result),
    do: Application.put_env(:wocky, :sms_send_result, result)
end
