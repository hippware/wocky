defmodule Wocky.PigeonConfig do
  @moduledoc "Config callback module for Pigeon"

  alias Pigeon.APNS.JWTConfig, as: APNS
  alias Pigeon.FCM.Config, as: FCM

  @spec apns_config :: APNS.t()
  def apns_config do
    :wocky
    |> Confex.get_env(:pigeon)
    |> Keyword.get(:apns, [])
    |> Keyword.merge(name: :apns_default)
    |> APNS.new()
  end

  @spec fcm_config :: FCM.t()
  def fcm_config do
    :wocky
    |> Confex.get_env(:pigeon)
    |> Keyword.get(:fcm, [])
    |> Keyword.merge(name: :fcm_default)
    |> FCM.new()
  end
end
