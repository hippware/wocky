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
        token = Wocky.Repo.Factory.get_test_token(user)
        {:ok, socket!} = Phoenix.ChannelTest.connect(WockyAPI.UserSocket, %{})
        {:ok, socket!} = Absinthe.Phoenix.SubscriptionTest.join_absinthe(socket!)
        {:ok, socket: socket!, user: user, token: token}
      end
    end
  end
end
