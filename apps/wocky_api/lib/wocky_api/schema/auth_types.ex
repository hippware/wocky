defmodule WockyAPI.Schema.AuthTypes do
  @moduledoc """
  Absinthe types for wocky authentication
  """

  use Absinthe.Schema.Notation

  alias WockyAPI.Resolvers.Auth

  object :auth_mutations do
    field :authenticate, :user do
      arg :user, non_null(:string)
      arg :token, non_null(:string)
      resolve &Auth.authenticate/3
      middleware fn res, _ ->
        with %{value: user} <- res do
          %{res | context: Map.put(res.context, :current_user, user)}
        end
      end
    end
  end
end
