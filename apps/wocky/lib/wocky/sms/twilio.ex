defmodule Wocky.SMS.Twilio do
  @moduledoc "Implements the SMS Messenger behavior with Twilio"

  @behaviour Wocky.SMS.Messenger

  alias ExTwilio.Message

  def send(recipient, body) do
    sender = Confex.get_env(:wocky, :twilio_number)

    with {:ok, _} <- Message.create(to: recipient, from: sender, body: body) do
      :ok
    end
  end
end
