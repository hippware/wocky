defmodule WockyAPI.Schema.AuthTypes do
  @moduledoc """
  Absinthe types for wocky authentication
  """

  use WockyAPI.Schema.Notation

  alias WockyAPI.LoggingSocket
  alias WockyAPI.Metrics
  alias WockyAPI.Resolvers.Auth

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

      middleware fn res, _ ->
        with %{value: %{user: user, device: device}} <- res do
          transport_pid = res.context[:transport_pid]

          if transport_pid do
            LoggingSocket.set_user_info(transport_pid, user, device)
            Metrics.add_auth_connection(transport_pid)
          end

          %{res | context: Map.put(res.context, :current_user, user)}
        end
      end
    end
  end
end
