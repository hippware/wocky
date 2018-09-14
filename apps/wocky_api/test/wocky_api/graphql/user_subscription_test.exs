defmodule WockyAPI.GraphQL.UserSubscriptionTest do
  use WockyAPI.SubscriptionCase, async: false

  import WockyAPI.ChannelHelper
  import WockyAPI.GraphQLHelper

  alias Wocky.Repo

  setup_all do
    Ecto.Adapters.SQL.Sandbox.mode(Repo, :auto)
  end

  test "deleted user should not have further access",
       %{
         socket: socket,
         user: %{id: user_id},
         token: token
       } do
    authenticate(user_id, token, socket)
    query = "mutation { userDelete { result } }"
    ref = push_doc(socket, query)
    assert_reply ref, :ok, reply, 1000
    refute has_errors(reply)
    assert reply.data == %{"userDelete" => %{"result" => true}}

    query = "query { currentUser { id } }"
    ref = push_doc(socket, query)
    assert_reply ref, :ok, reply, 1000
    assert has_errors(reply)
    assert error_msg(reply) =~ "This operation requires an authenticated user"
  end
end
