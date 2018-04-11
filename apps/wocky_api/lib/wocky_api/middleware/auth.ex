defmodule WockyAPI.Middleware.Auth do
  @moduledoc """
  Absinthe middleware to handle standard authentication.
  """

  @behaviour Absinthe.Middleware

  def call(%{context: %{current_user: _}} = resolution, _config) do
    resolution
  end

  def call(resolution, _config) do
    Absinthe.Resolution.put_result(
      resolution,
      {:error, "This operation requires an authenticated user"})
  end
end
