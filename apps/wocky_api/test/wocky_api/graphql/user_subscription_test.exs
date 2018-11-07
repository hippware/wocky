defmodule WockyAPI.GraphQL.UserSubscriptionTest do
  use WockyAPI.SubscriptionCase, async: false

  import WockyAPI.ChannelHelper
  import WockyAPI.GraphQLHelper

  alias Wocky.Repo.Factory
  alias Wocky.{Roster, User}

  setup_all do
    setup_watcher()
  end

  setup %{socket: socket, user: user, token: token} do
    authenticate(user.id, token, socket)
    :ok
  end

  test "deleted user should not have further access", %{socket: socket} do
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

  describe "followee subscription" do
    @query """
    subscription {
      followees {
        id
        handle
        presence_status
      }
    }
    """

    setup %{user: user, socket: socket} do
      [follower, followee, stranger] = Factory.insert_list(3, :user)

      Roster.follow(follower.id, user.id)
      Roster.follow(user.id, followee.id)

      ref = push_doc(socket, @query)
      assert_reply ref, :ok, %{subscriptionId: subscription_id}, 1000

      {:ok,
       follower: follower,
       followee: followee,
       stranger: stranger,
       subscription_id: subscription_id}
    end

    test "updating a followee sends a message", %{
      followee: followee,
      subscription_id: subscription_id
    } do
      new_handle = Factory.new_handle()
      User.update(followee, %{handle: new_handle})

      assert_push "subscription:data", push, 2000

      assert push == %{
               result: %{
                 data: %{
                   "followees" => %{
                     "handle" => new_handle,
                     "id" => followee.id,
                     "presence_status" => "OFFLINE"
                   }
                 }
               },
               subscriptionId: subscription_id
             }
    end

    test "updating a follower sends no message", %{follower: follower} do
      User.update(follower, %{handle: Factory.new_handle()})

      refute_push "subscription:data", _push, 500
    end

    test "updating a stranger sends no message", %{stranger: stranger} do
      User.update(stranger, %{handle: Factory.new_handle()})

      refute_push "subscription:data", _push, 500
    end

    test "updating ourself sends no message", %{user: user} do
      User.update(user, %{handle: Factory.new_handle()})

      refute_push "subscription:data", _push, 500
    end
  end
end
