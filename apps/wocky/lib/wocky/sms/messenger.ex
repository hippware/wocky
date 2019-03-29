defmodule Wocky.SMS.Messenger do
  @moduledoc "Behavior for modules providing SMS messaging capabilities."

  @callback send(binary, binary) :: :ok

  alias Wocky.User

  @spec send(binary, binary, User.t()) :: :ok | {:error, term()}
  def send(recipient, body, sender) do
    if User.sms_allowed_inc?(sender, recipient) do
      backend = Confex.get_env(:wocky, :sms_backend)
      backend.send(recipient, body)
    else
      {:error, :user_sms_limit_reached}
    end
  end
end
