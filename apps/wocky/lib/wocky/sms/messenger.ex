defmodule Wocky.SMS.Messenger do
  @moduledoc "Behavior for modules providing SMS messaging capabilities."

  @callback send(binary, binary) :: :ok

  @spec send(binary, binary) :: :ok
  def send(recipient, body) do
    backend = Confex.get_env(:wocky, :sms_backend)
    backend.send(recipient, body)
  end
end
