defmodule Wocky.Account.JWT.SigningKey do
  @moduledoc """
  Fetch JWT signing keys from the environment.
  """

  @spec fetch(:client | :server) :: binary()
  def fetch(key_id) do
    varname = varname(key_id)

    Confex.get_env(:wocky, varname)
  end

  defp varname(:client), do: :client_jwt_signing_key
  defp varname(:server), do: :server_jwt_signing_key
end
