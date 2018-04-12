defmodule WockyAPI.Schema.AuthTypes do
  @moduledoc """
  Absinthe types for wocky authentication
  """

  use Absinthe.Schema.Notation
  use Absinthe.Relay.Schema.Notation, :modern

  alias WockyAPI.Resolvers.Auth

  object :auth_mutations do
    payload field :authenticate do
      input do
        field :user, :string
        field :token, non_null(:string)
      end
      output do
        field :user, non_null(:user)
      end
      resolve &Auth.authenticate/3
      middleware fn res, _ ->
        with %{value: %{user: user}} <- res do
          %{res | context: Map.put(res.context, :current_user, user)}
        end
      end
    end
  end
end
