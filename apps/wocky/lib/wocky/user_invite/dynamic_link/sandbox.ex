defmodule Wocky.UserInvite.DynamicLink.Sandbox do
  @moduledoc """
  Sandbox module for testing dynamic links
  """

  @behaviour Wocky.UserInvite.DynamicLink

  @impl true
  def invitation_link(invitation_code) do
    case Confex.get_env(:wocky, :invitation_link_result, :ok) do
      :ok -> {:ok, "https://tinyrobot.com/?invitation" <> invitation_code}
      error -> error
    end
  end

  def set_result(result),
    do: Application.put_env(:wocky, :invitation_link_result, result)
end
