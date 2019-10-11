defmodule WockyAPI.Test.Query.Auth do
  @moduledoc false

  def authenticate do
    """
    mutation ($token: String!) {
    authenticate (input: {token: $token}) {
        user {
          id
        }
      }
    }
    """
  end
end
