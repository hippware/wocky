defmodule WockyAPI.GraphQL.UserCallbackTest do
  use WockyAPI.SubscriptionCase, async: false

  import WockyAPI.GraphQLHelper

  alias Wocky.Presence.Manager

  setup_all do
    require_watcher()

    WockyAPI.Callbacks.User.register()
  end

  setup %{socket: socket, user: user, token: token} do
    authenticate(user.id, token, socket)

    on_exit(fn ->
      Manager.stop_all()
    end)

    :ok
  end

  test "deleted user should not have further access", %{socket: socket} do
    delete = "mutation { userDelete { result } }"
    ref! = push_doc(socket, delete)
    assert_reply ref!, :ok, reply, 1000
    refute has_errors(reply)
    assert reply.data == %{"userDelete" => %{"result" => true}}

    query = "query { currentUser { id } }"
    ref! = push_doc(socket, query)
    assert_reply ref!, :ok, reply, 1000
    assert has_errors(reply)
    assert error_msg(reply) =~ "This operation requires an authenticated user"
  end
end
