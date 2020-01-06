defmodule WockyAPI.GraphQL.WebsocketTest do
  use WockyAPI.GraphQLCase, async: false

  import Eventually

  alias AbsintheWebSocket.WebSocket
  alias Ecto.Adapters.SQL.Sandbox, as: SQLSandbox
  alias Wocky.Account
  alias Wocky.Presence
  alias Wocky.Repo
  alias WockyAPI.Factory, as: APIFactory
  alias WockyAPI.Test.Client
  alias WockyAPI.Test.Context

  setup_all do
    SQLSandbox.mode(Repo, :auto)

    {:ok, sup} = start_supervised(Client.supervisor())
    Client.init()

    {:ok, sup: sup}
  end

  setup ctx do
    require_watcher()
    WockyAPI.Callbacks.User.register()

    user = Factory.insert(:user)

    jwt = APIFactory.get_test_token(user)

    await_ready(get_client_socket(ctx.sup))
    WebSocket.set_opt(get_client_socket(ctx.sup), :disconnect_sleep, 0)
    WebSocket.set_opt(get_client_socket(ctx.sup), :log_disconnect, false)

    on_exit(fn ->
      Supervisor.terminate_child(ctx.sup, WebSocket)
      Supervisor.restart_child(ctx.sup, WebSocket)
      Dawdle.Client.clear_all_handlers()
      Wocky.Repo.delete_all(Wocky.Account.User)
    end)

    {:ok, user: user, jwt: jwt}
  end

  defp await_ready(socket) do
    case :sys.get_state(socket).ready do
      true ->
        :ok

      false ->
        Process.sleep(50)
        await_ready(socket)
    end
  end

  describe "user socket tracking" do
    test "an unconnected user should have no sockets", ctx do
      assert_eventually(Presence.get_sockets(ctx.user) == [], 500)
    end

    test "an authenticated user should have a socket", ctx do
      {:ok, _, nil} = Context.authenticate(ctx.jwt)

      assert_eventually(length(Presence.get_sockets(ctx.user)) == 1, 500)
    end

    test "disconnection should remove the socket from the list", ctx do
      {:ok, _, nil} = Context.authenticate(ctx.jwt)

      WebSocket.close(get_client_socket(ctx.sup))

      assert_eventually(Presence.get_sockets(ctx.user) == [], 500)
    end

    test "an unconnected user should return false", ctx do
      refute_eventually(Presence.connected?(ctx.user), 500)
    end

    test "an authenticated user shoult return true", ctx do
      {:ok, _, nil} = Context.authenticate(ctx.jwt)

      assert_eventually(Presence.connected?(ctx.user), 500)
    end

    test "a disconnected user should return false", ctx do
      {:ok, _, nil} = Context.authenticate(ctx.jwt)

      WebSocket.close(get_client_socket(ctx.sup))
      refute_eventually(Presence.connected?(ctx.user), 500)
    end
  end

  describe "user deletion" do
    setup ctx do
      self = self()

      get_socket()
      |> AbsintheWebSocket.WebSocket.set_opt(:disconnect_callback, fn ->
        disconnect_callback(self)
      end)

      {:ok, _, nil} = Context.authenticate(ctx.jwt)

      :ok
    end

    test "socket closes on user deletion via API" do
      {:ok, _, nil} = Context.user_delete()

      assert_receive :disconnected, 500
    end

    test "socket closes on user deletion via DB", ctx do
      Account.delete(ctx.user)

      assert_receive :disconnected, 500
    end
  end

  def disconnect_callback(target), do: send(target, :disconnected)

  defp get_socket do
    query_server_name = Module.safe_concat([Client.mod(), Caller, QueryServer])
    :sys.get_state(query_server_name).socket
  end

  defp get_client_socket(sup) do
    sup
    |> Supervisor.which_children()
    |> List.keyfind(WebSocket, 0)
    |> elem(1)
  end
end
