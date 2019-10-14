defmodule WockyAPI.GraphQL.WebsocketTest do
  use WockyAPI.GraphQLCase, async: false

  import Eventually

  alias AbsintheWebSocket.WebSocket
  alias Ecto.Adapters.SQL.Sandbox, as: SQLSandbox
  alias Wocky.Presence
  alias Wocky.Repo
  alias Wocky.Repo.Factory
  alias WockyAPI.Factory, as: APIFactory
  alias WockyAPI.Test.Client
  alias WockyAPI.Test.Context

  setup_all do
    SQLSandbox.mode(Repo, :auto)
    {:ok, sup} = start_supervised(Client.supervisor())
    Client.init()
    WebSocket.set_opt(get_client_socket(sup), :disconnect_sleep, 0)

    {:ok, user: Factory.insert(:user), sup: sup}
  end

  setup ctx do
    await_ready(get_client_socket(ctx.sup))

    on_exit(fn ->
      Supervisor.terminate_child(ctx.sup, WebSocket)
      Supervisor.restart_child(ctx.sup, WebSocket)
    end)

    :ok
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
      assert_eventually(Presence.get_sockets(ctx.user) == [])
    end

    test "an authenticated user should have a socket", ctx do
      jwt = APIFactory.get_test_token(ctx.user)
      {:ok, _, nil} = Context.authenticate(jwt)

      assert [pid] = Presence.get_sockets(ctx.user)
      assert is_pid(pid)
    end

    test "disconnection should remove the socket from the list", ctx do
      jwt = APIFactory.get_test_token(ctx.user)
      {:ok, _, nil} = Context.authenticate(jwt)

      WebSocket.close(get_client_socket(ctx.sup))

      assert_eventually(Presence.get_sockets(ctx.user) == [])
    end

    test "an unconnected user should return false", ctx do
      refute_eventually(Presence.connected?(ctx.user))
    end

    test "an authenticated user shoult return true", ctx do
      jwt = APIFactory.get_test_token(ctx.user)
      {:ok, _, nil} = Context.authenticate(jwt)

      assert Presence.connected?(ctx.user)
    end

    test "a disconnected user should return false", ctx do
      jwt = APIFactory.get_test_token(ctx.user)
      {:ok, _, nil} = Context.authenticate(jwt)

      WebSocket.close(get_client_socket(ctx.sup))
      refute_eventually(Presence.connected?(ctx.user))
    end
  end

  defp get_client_socket(sup) do
    sup
    |> Supervisor.which_children()
    |> List.keyfind(WebSocket, 0)
    |> elem(1)
  end
end
