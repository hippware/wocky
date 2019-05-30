defmodule WockyAPI.GraphQL.UserSubscriptionTest do
  use WockyAPI.SubscriptionCase, async: false

  import Eventually
  import WockyAPI.ChannelHelper
  import WockyAPI.GraphQLHelper

  alias Wocky.Repo.{Factory, Timestamp}
  alias Wocky.{Roster, User}
  alias Wocky.User.LocationShare.Cache

  setup_all :require_watcher

  setup %{socket: socket, user: user, token: token} do
    authenticate(user.id, token, socket)
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

      Roster.befriend(friend, user)

      ref = push_doc(socket, @query)
      assert_reply ref, :ok, %{subscriptionId: subscription_id}, 1000

      {:ok,
       friend: friend, stranger: stranger, subscription_id: subscription_id}
    end

    test "updating a friend sends a message", %{
      friend: friend,
      subscription_id: subscription_id
    } do
      new_handle = Factory.handle()
      User.update(friend, %{handle: new_handle})

      assert_push "subscription:data", push, 2000

      assert push == %{
               result: %{
                 data: %{
                   "friends" => %{
                     "handle" => new_handle,
                     "id" => friend.id,
                     "presence_status" => "OFFLINE"
                   }
                 }
               },
               subscriptionId: subscription_id
             }
    end

    test "updating a stranger sends no message", %{stranger: stranger} do
      User.update(stranger, %{handle: Factory.handle()})

      refute_push "subscription:data", _push, 500
    end

    test "updating ourself sends no message", %{user: user} do
      User.update(user, %{handle: Factory.handle()})

      refute_push "subscription:data", _push, 500
    end
  end

  describe "shared_locations subscription" do
    @query """
    subscription {
      sharedLocations {
        user {
          id
        }
        location {
          lat
          lon
          accuracy
          capturedAt
        }
      }
    }
    """

    setup %{user: user} do
      expiry = Timestamp.shift(days: 5)
      friend = Factory.insert(:user)

      Roster.befriend(friend, user)
      User.start_sharing_location(friend, user, expiry)

      # Wait for the cache to catch up
      assert_eventually(length(Cache.get(friend.id)) == 1)

      {:ok, friend: friend}
    end

    test "updating location sends a message", %{socket: socket, friend: friend} do
      ref = push_doc(socket, @query)
      assert_reply ref, :ok, %{subscriptionId: subscription_id}, 1000

      captured_at = DateTime.utc_now() |> DateTime.to_iso8601()
      location = Factory.build(:location, captured_at: captured_at)
      {:ok, loc} = User.set_location(friend, location)

      assert_push "subscription:data", push, 2000

      id = friend.id
      accuracy = loc.accuracy

      assert %{
               result: %{
                 data: %{
                   "sharedLocations" => %{
                     "user" => %{
                       "id" => ^id
                     },
                     "location" => %{
                       "lat" => lat,
                       "lon" => lon,
                       "accuracy" => ^accuracy,
                       "capturedAt" => ^captured_at
                     }
                   }
                 }
               },
               subscriptionId: ^subscription_id
             } = push

      assert Float.round(lat, 8) == Float.round(loc.lat, 8)
      assert Float.round(lon, 8) == Float.round(loc.lon, 8)
    end

    test "location catchup", %{socket: socket, friend: friend} do
      captured_at = DateTime.utc_now() |> DateTime.to_iso8601()
      location = Factory.build(:location, captured_at: captured_at)
      {:ok, loc} = User.set_location(friend, location)

      ref = push_doc(socket, @query)
      assert_reply ref, :ok, %{subscriptionId: subscription_id}, 1000

      assert_push "subscription:data", push, 2000

      id = friend.id
      accuracy = loc.accuracy

      assert %{
               result: %{
                 data: %{
                   "sharedLocations" => %{
                     "user" => %{
                       "id" => ^id
                     },
                     "location" => %{
                       "lat" => lat,
                       "lon" => lon,
                       "accuracy" => ^accuracy,
                       "capturedAt" => ^captured_at
                     }
                   }
                 }
               },
               subscriptionId: ^subscription_id
             } = push

      assert Float.round(lat, 8) == Float.round(loc.lat, 8)
      assert Float.round(lon, 8) == Float.round(loc.lon, 8)
    end
  end
end
