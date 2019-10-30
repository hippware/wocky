defmodule WockyAPI.GraphQL.UserSubscriptionTest do
  use WockyAPI.SubscriptionCase, async: false

  import Eventually
  import WockyAPI.GraphQLHelper

  alias Wocky.Account
  alias Wocky.Friends
  alias Wocky.Friends.Share.Cache
  alias Wocky.Location
  alias Wocky.Repo.Factory

  setup_all do
    require_watcher()

    WockyAPI.Callbacks.LocationChanged.register()
    WockyAPI.Callbacks.Friend.register()
    WockyAPI.Callbacks.User.register()
  end

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

      Friends.befriend(friend, user)

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
      friend = Factory.insert(:user)

      Friends.befriend(friend, user)
      Friends.update_sharing(friend, user, :always)

      # Wait for the cache to catch up
      assert_eventually(length(Cache.get(friend.id)) == 1)

      {:ok, friend: friend}
    end

    test "updating location sends a message", %{socket: socket, friend: friend} do
      ref = push_doc(socket, @query)
      assert_reply ref, :ok, %{subscriptionId: subscription_id}, 150

      now = DateTime.utc_now()
      captured_at = now |> DateTime.to_iso8601()
      location = Factory.build(:location, captured_at: now)
      {:ok, loc} = Location.set_user_location(friend, location)

      id = friend.id
      accuracy = loc.accuracy

      assert_subscription_update %{
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
      }

      assert Float.round(lat, 8) == Float.round(loc.lat, 8)
      assert Float.round(lon, 8) == Float.round(loc.lon, 8)
    end

    test "location catchup", %{socket: socket, friend: friend} do
      now = DateTime.utc_now()
      captured_at = now |> DateTime.to_iso8601()
      location = Factory.build(:location, captured_at: now)
      {:ok, loc} = Location.set_user_location(friend, location)

      ref = push_doc(socket, @query)
      assert_reply ref, :ok, %{subscriptionId: subscription_id}, 150

      id = friend.id
      accuracy = loc.accuracy

      assert_subscription_update %{
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
      }

      assert Float.round(lat, 8) == Float.round(loc.lat, 8)
      assert Float.round(lon, 8) == Float.round(loc.lon, 8)
    end
  end
end
