defmodule WockyAPI.Middleware.RefreshCurrentUser do
  @moduledoc """
  Absinthe middleware to refresh the context when the `current_user` struct
  has changed.
  """

  @behaviour Absinthe.Middleware

  def call(%{
    context: %{current_user: %Wocky.User{id: id} = _} = context,
    value: %Wocky.User{id: id} = user
  } = resolution, _) do
    %{resolution | context: %{context | current_user: user}}
  end

  def call(resolution, _) do
    resolution
  end
end
