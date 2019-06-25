defmodule WockyAPI.SubscriptionCase do
  @moduledoc """
  Test case framework for Absinthe subscription tests
  """

  use ExUnit.CaseTemplate

  using do
    quote do
      use WockyAPI.ChannelCase
      use Absinthe.Phoenix.SubscriptionTest, schema: WockyAPI.Schema

      import WockyAPI.WatcherHelper
      import unquote(__MODULE__)

      setup do
        user = Wocky.Repo.Factory.insert(:user)
        token = WockyAPI.Factory.get_test_token(user)

        {:ok, socket!} =
          Phoenix.ChannelTest.connect(WockyAPI.Channels.UserSocket, %{})

        {:ok, socket!} =
          Absinthe.Phoenix.SubscriptionTest.join_absinthe(socket!)

        {:ok, socket: socket!, user: user, token: token}
      end
    end
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
