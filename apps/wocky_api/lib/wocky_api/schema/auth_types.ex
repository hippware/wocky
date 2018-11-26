defmodule WockyAPI.Schema.AuthTypes do
  @moduledoc """
  Absinthe types for wocky authentication
  """

  use WockyAPI.Schema.Notation

  alias WockyAPI.Resolvers.Auth
  alias WockyAPI.Transports.LoggingWebSocket

  @desc "Authenticate a user to the GraphQL interface"
  object :auth_mutations do
    payload field :authenticate do
      scope :public

      input do
        @desc "The ID of the user to authenticate"
        field :user, :string

        @desc "The token (legacy Wocky or JWT) with which to authenticate"
        field :token, non_null(:string)
      end

      output do
        @desc "The authenticated user"
        field :user, non_null(:user)
      end

      resolve &Auth.authenticate/3

      middleware fn res, _ ->
        with %{value: %{user: user}} <- res do
          transport_pid = res.context[:transport_pid]
          if transport_pid, do: LoggingWebSocket.set_user(transport_pid, user)

          %{res | context: Map.put(res.context, :current_user, user)}
        end
      end
    end
  end
end
