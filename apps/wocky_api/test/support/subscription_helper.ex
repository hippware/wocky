defmodule WockyAPI.SubscriptionHelper do
  @moduledoc """
  Helper functions for graphql channel tests
  """

  use Phoenix.ChannelTest

  import Absinthe.Phoenix.SubscriptionTest
  import ExUnit.Assertions

  def authenticate(user_id, token, socket) do
    authenticate = """
      mutation ($input: AuthenticateInput) {
        authenticate(input: $input) {
          user {
            id
          }
        }
      }
    """

    ref =
      push_doc(
        socket,
        authenticate,
        variables: %{input: %{token: token}}
      )

    assert_reply ref,
                 :ok,
                 %{
                   data: %{"authenticate" => %{"user" => %{"id" => ^user_id}}}
                 },
                 150

    ref
  end

  defmacro assert_subscription_update(pattern, timeout \\ 150) do
    quote do
      # NOTE I have left the timing code intact but commented out in
      # case we need it again.

      # start = DateTime.utc_now()
      assert_push "subscription:data", unquote(pattern), unquote(timeout)
      # time = DateTime.diff(DateTime.utc_now(), start, :millisecond)
      # IO.puts("Subscription data received in #{time}ms")
    end
  end

  defmacro refute_subscription_update(pattern, timeout \\ 150) do
    quote do
      refute_push "subscription:data", unquote(pattern), unquote(timeout)
    end
  end
end
