defmodule Wocky.Account.JWT.SigningKey do
  @moduledoc """
  Fetch JWT signing keys from the environment.
  """

  @spec fetch(:client | :server) :: binary()
  def fetch(key_id) do
    varname = varname(key_id)

    if varname do
      Confex.get_env(:wocky, varname)
    end
  end

  defp varname(:client), do: :client_jwt_signing_key
  defp varname(:server), do: :server_jwt_signing_key
end
