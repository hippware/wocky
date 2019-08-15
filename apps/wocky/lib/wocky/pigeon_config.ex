defmodule Wocky.PigeonConfig do
  @moduledoc "Config callback module for Pigeon"

  alias Pigeon.APNS.JWTConfig, as: APNS
  alias Pigeon.FCM.Config, as: FCM

  def apns_config do
    :wocky
    |> Confex.get_env(:pigeon)
    |> Keyword.get(:apns, [])
    |> Keyword.merge(name: :apns_default)
    |> APNS.new()
  end

  def fcm_config do
    :wocky
    |> Confex.get_env(:pigeon)
    |> Keyword.get(:fcm, [])
    |> Keyword.merge(name: :fcm_default)
    |> FCM.new()
  end
end
