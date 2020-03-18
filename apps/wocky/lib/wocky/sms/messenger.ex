defmodule Wocky.SMS.Messenger do
  @moduledoc "Behavior for modules providing SMS messaging capabilities."

  alias Wocky.Account
  alias Wocky.Account.User

  @callback send(String.t(), String.t()) :: :ok | {:error, any()}

  @spec send(String.t(), String.t(), User.t()) :: :ok | {:error, any()}
  def send(recipient, body, sender) do
    if Account.sms_allowed_inc?(sender) do
      backend = Confex.get_env(:wocky, :sms_backend)
      backend.send(recipient, body)
    else
      {:error, :user_sms_limit_reached}
    end
  end
end
