defmodule WockyAPI.SubscriptionCase do
  @moduledoc """
  Test case framework for Absinthe subscription tests
  """

  use ExUnit.CaseTemplate

  using do
    quote do
      alias Wocky.Repo.Factory

      # Import conveniences for testing with channels
      use Phoenix.ChannelTest

      import Absinthe.Phoenix.SubscriptionTest
      import WockyAPI.SubscriptionHelper
      import WockyAPI.WatcherHelper

      # The default endpoint for testing
      @endpoint WockyAPI.Endpoint

      setup do
        user = Wocky.Repo.Factory.insert(:user)
        token = WockyAPI.Factory.get_test_token(user)

        {:ok, socket!} = connect(WockyAPI.Channels.UserSocket, %{})
        {:ok, socket!} = join_absinthe(socket!)

        {:ok, socket: socket!, user: user, token: token}
      end
    end
  end
end
