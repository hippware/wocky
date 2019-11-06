defmodule WockyAPI.GraphQL.LocationSharingTest do
  use WockyAPI.SubscriptionCase, async: false

  import Eventually

  alias Wocky.Friends
  alias Wocky.Friends.Share.Cache
  alias Wocky.Location
  alias Wocky.Repo.Factory

  setup_all do
    require_watcher()

    Wocky.Callbacks.Friend.register()
    WockyAPI.Callbacks.LocationChanged.register()
    WockyAPI.Callbacks.Friend.register()
    WockyAPI.Callbacks.User.register()
  end

  setup %{socket: socket, user: user, token: token} do
    authenticate(user.id, token, socket)
    :ok
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

      Friends.befriend(friend, user, share_type: :always)

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

    test "updating location sends a message in `nearby` mode", %{
      socket: socket,
      user: user,
      friend: friend
    } do
      Friends.update_sharing(friend, user, :nearby)
      assert_eventually(hd(Cache.get(friend.id)).share_type == :nearby)

      ref = push_doc(socket, @query)
      assert_reply ref, :ok, %{subscriptionId: subscription_id}, 150
      now = DateTime.utc_now()

      location =
        Factory.build(:location,
          user_id: user.id,
          lat: 0.0,
          lon: 0.0,
          captured_at: now
        )

      {:ok, _} = Location.set_user_location(user, location)

      {:ok, loc} =
        Location.set_user_location(friend, %{location | user_id: friend.id})

      captured_at = now |> DateTime.to_iso8601()
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
                "lat" => 0.0,
                "lon" => 0.0,
                "accuracy" => ^accuracy,
                "capturedAt" => ^captured_at
              }
            }
          }
        },
        subscriptionId: ^subscription_id
      }
    end

    test "updating location does not send a message in `nearby` mode when outside the nearby range",
         %{socket: socket, user: user, friend: friend} do
      Friends.update_sharing(friend, user, :nearby)
      assert_eventually(hd(Cache.get(friend.id)).share_type == :nearby)

      ref = push_doc(socket, @query)
      assert_reply ref, :ok, %{subscriptionId: subscription_id}, 150
      now = DateTime.utc_now()

      location =
        Factory.build(:location,
          user_id: user.id,
          lat: 50.0,
          lon: 50.0,
          captured_at: now
        )

      {:ok, _} = Location.set_user_location(user, location)

      location2 =
        Factory.build(:location,
          user_id: friend.id,
          lat: 0.0,
          lon: 0.0,
          captured_at: now
        )

      {:ok, _} = Location.set_user_location(friend, location2)

      refute_subscription_update _data
    end

    test "updating location does not send a message in `nearby` mode when other user has no registered location",
         %{socket: socket, user: user, friend: friend} do
      Friends.update_sharing(friend, user, :nearby)
      assert_eventually(hd(Cache.get(friend.id)).share_type == :nearby)

      ref = push_doc(socket, @query)
      assert_reply ref, :ok, %{subscriptionId: subscription_id}, 150
      now = DateTime.utc_now()

      location =
        Factory.build(:location,
          user_id: friend.id,
          lat: 0.0,
          lon: 0.0,
          captured_at: now
        )

      {:ok, _} = Location.set_user_location(friend, location)

      refute_subscription_update _data
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
