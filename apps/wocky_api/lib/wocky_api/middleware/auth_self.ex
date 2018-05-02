defmodule WockyAPI.Middleware.AuthSelf do
  @moduledoc """
  Absinthe middlware to handle authorization where the
  field/object is only accessible by the object's owner.
  """

  @behaviour Absinthe.Middleware

  alias Wocky.User

  def call(
        %{context: %{current_user: %User{id: id}}, source: %User{id: id}} =
          resolution,
        _config
      ) do
    resolution
  end

  def call(resolution, _config) do
    Absinthe.Resolution.put_result(
      resolution,
      {:error, "This query can only be made against the authenticated user"}
    )
  end
end
