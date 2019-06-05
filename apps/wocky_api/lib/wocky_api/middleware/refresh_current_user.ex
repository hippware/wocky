defmodule WockyAPI.Middleware.RefreshCurrentUser do
  @moduledoc """
  Absinthe middleware to refresh the context when the `current_user` struct
  has changed.
  """

  @behaviour Absinthe.Middleware

  alias Wocky.Account
  alias Wocky.Account.User

  # If the mutation provided a new user object, just drop that in
  def call(
        %{
          context: %{current_user: %User{id: id}} = context,
          value: %User{id: id} = user
        } = resolution,
        _
      ) do
    %{resolution | context: %{context | current_user: user}}
  end

  # For all other user mutation types, reload the user from the DB
  def call(
        %{
          context: %{current_user: %User{id: id} = user} = context
        } = resolution,
        _
      ) do
    %{
      resolution
      | context:
          case Account.get_user(id, user) do
            nil -> Map.delete(context, :current_user)
            updated_user -> %{context | current_user: updated_user}
          end
    }
  end

  # No action if we don't have a current user
  def call(resolution, _), do: resolution
end
