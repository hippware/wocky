defmodule Wocky.UserInvite.DynamicLink do
  @moduledoc """
  Interface module for dynamic link generation
  """

  @callback invitation_link(binary()) :: {:ok, binary()} | {:error, term()}

  @spec invitation_link(binary()) :: {:ok, binary()} | {:error, term()}
  def invitation_link(code) do
    backend = Confex.get_env(:wocky, :dynamic_link_backend)

    backend.invitation_link(code)
  end
end
