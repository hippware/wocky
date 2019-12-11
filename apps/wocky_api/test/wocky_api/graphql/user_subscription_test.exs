defmodule WockyAPI.GraphQL.UserSubscriptionTest do
  use WockyAPI.SubscriptionCase, async: false

  import WockyAPI.GraphQLHelper

  alias Wocky.Account
  alias Wocky.Contacts
  alias Wocky.Presence.Manager
  alias Wocky.Repo.Factory

  setup_all do
    require_watcher()

    WockyAPI.Callbacks.Relationship.register()
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

  describe "friends subscription" do
    @query """
    subscription {
      friends {
        id
        handle
        presence_status
      }
    }
    """

    setup %{user: user, socket: socket} do
      [friend, stranger] = Factory.insert_list(2, :user)

      Contacts.befriend(friend, user)

      ref = push_doc(socket, @query)
      assert_reply ref, :ok, %{subscriptionId: subscription_id}, 150

      {:ok,
       friend: friend, stranger: stranger, subscription_id: subscription_id}
    end

    test "updating a friend sends a message", %{
      friend: friend,
      subscription_id: subscription_id
    } do
      friend_id = friend.id
      new_handle = Factory.handle()
      Account.update(friend, %{handle: new_handle})

      assert_subscription_update %{
        result: %{
          data: %{
            "friends" => %{
              "handle" => ^new_handle,
              "id" => ^friend_id,
              "presence_status" => "OFFLINE"
            }
          }
        },
        subscriptionId: ^subscription_id
      }
    end

    test "updating a stranger sends no message", %{stranger: stranger} do
      Account.update(stranger, %{handle: Factory.handle()})

      refute_subscription_update _data
    end

    test "updating ourself sends no message", %{user: user} do
      Account.update(user, %{handle: Factory.handle()})

      refute_subscription_update _data
    end
  end
end
