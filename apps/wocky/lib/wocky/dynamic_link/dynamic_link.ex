defmodule Wocky.DynamicLink do
  @moduledoc """
  Interface module for dynamic link generation
  """

  alias Wocky.User

  @callback invitation_link(binary()) :: {:ok, binary()} | {:error, term()}

  @spec invitation_link(User.t()) :: {:ok, binary()} | {:error, term()}
  def invitation_link(user) do
    backend = Confex.get_env(:wocky, :dynamic_link_backend)

    user
    |> User.make_invite_code()
    |> backend.invitation_link()
  end
end
