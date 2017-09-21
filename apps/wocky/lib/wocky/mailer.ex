defmodule Wocky.Mailer do
  @moduledoc """
  Mailer module to configure Bamboo for Wocky
  """

  use Bamboo.Mailer, otp_app: :wocky

  def init do
    new_vals =
      :wocky
      |> Application.fetch_env!(__MODULE__)
      |> Confex.process_env

    Application.put_env(:wocky, __MODULE__, new_vals)
  end
end
