defmodule WockyAPI.Middleware.AuthUser do
  @moduledoc """
  Absinthe middleware to handle standard authorization.
  """

  @behaviour Absinthe.Middleware

  @public_modules [
    Absinthe.Phase.Schema.Introspection,
    Absinthe.Type.BuiltIns.Introspection
  ]

  @impl true
  def call(%{context: %{current_user: _}} = resolution, _config) do
    resolution
  end

  @impl true
  def call(resolution, _config) do
    module = resolution.definition.schema_node.__reference__.module

    if module in @public_modules do
      resolution
    else
      Absinthe.Resolution.put_result(
        resolution,
        {:error, "This operation requires an authenticated user"}
      )
    end
  end
end
