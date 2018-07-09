defmodule WockyAPI.Middleware.RefreshCurrentUser do
  @moduledoc """
  Absinthe middleware to refresh the context when the `current_user` struct
  has changed.
  """

  @behaviour Absinthe.Middleware

  alias Wocky.User

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
    _) do
    %{resolution | context: %{context | current_user: User.get_user(id, user)}}
  end
end
