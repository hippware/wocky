defmodule WockyAPI.SubscriptionCase do
  @moduledoc """
  Test case framework for Absinthe subscription tests
  """

  use ExUnit.CaseTemplate

  using do
    quote do
      use WockyAPI.ChannelCase
      use Absinthe.Phoenix.SubscriptionTest, schema: WockyAPI.Schema

      import unquote(__MODULE__)

      setup do
        user = Wocky.Repo.Factory.insert(:user)
        token = Wocky.Repo.Factory.get_test_token(user)
        {:ok, socket} = Phoenix.ChannelTest.connect(WockyAPI.UserSocket, %{})
        {:ok, socket} = Absinthe.Phoenix.SubscriptionTest.join_absinthe(socket)
        {:ok, socket: socket, user: user, token: token}
      end
    end
  end

  defmacro setup_watcher do
    quote do
      Wocky.Watcher.Client.clear_all_subscriptions()
      Wocky.Callbacks.register()
      WockyAPI.Callbacks.register()
      Ecto.Adapters.SQL.Sandbox.mode(Wocky.Repo, :auto)
      Application.start(:wocky_db_watcher)

      on_exit(fn ->
        Application.stop(:wocky_db_watcher)
        Wocky.Repo.delete_all(Wocky.User)
      end)
    end
  end
end
