defmodule WockyAPI.Schema.AuthTypes do
  @moduledoc """
  Absinthe types for wocky authentication
  """

  use WockyAPI.Schema.Notation

  alias WockyAPI.Middleware.Socket
  alias WockyAPI.Resolvers.Auth

  # -------------------------------------------------------------------
  # Mutations

  @desc "Authenticate a user to the GraphQL interface"
  object :auth_mutations do
    payload field :authenticate do
      scope :public

      input do
        @desc "The JWT with which to authenticate"
        field :token, non_null(:string)
      end

      output do
        @desc "The authenticated user"
        field :user, non_null(:user)
      end

      resolve &Auth.authenticate/3

      middleware Socket, :authenticated
    end
  end
end
