defmodule WockyAPI.GraphQL.SubscriptionTest do
  use WockyAPI.SubscriptionCase, async: false

  import WockyAPI.ChannelHelper

  alias Wocky.Bots
  alias Wocky.Bots.Subscription
  alias Wocky.GeoUtils
  alias Wocky.Repo.Factory
  alias Wocky.Roster

  setup_all :require_watcher

  describe "watch for visitor count change" do
    setup do
      user2 = Factory.insert(:user)
      bot = Factory.insert(:bot)
      Roster.befriend(bot.user, user2)
      Bots.subscribe(bot, user2)

      {:ok, user2: user2, bot: bot}
    end

    @subscription """
    subscription {
      botGuestVisitors {
        bot {
          id
          subscribers (first: 0, type: VISITOR) {
            totalCount
          }
        }
        visitor {
          id
        }
        action
        updated_at
      }
    }
    """
    test "visitor count changes", %{
      socket: socket,
      user2: user2,
      bot: bot,
      user: %{id: user_id} = user,
      token: token
    } do
      Roster.befriend(bot.user, user)
      Bots.subscribe(bot, user)

      authenticate(user_id, token, socket)

      ref = push_doc(socket, @subscription)
      assert_reply ref, :ok, %{subscriptionId: subscription_id}, 1000

      expected = fn count, action, updated_at ->
        %{
          result: %{
            data: %{
              "botGuestVisitors" => %{
                "bot" => %{
                  "id" => bot.id,
                  "subscribers" => %{
                    "totalCount" => count
                  }
                },
                "visitor" => %{
                  "id" => user2.id
                },
                "action" => action,
                "updated_at" => DateTime.to_iso8601(updated_at)
              }
            }
          },
          subscriptionId: subscription_id
        }
      end

      Bots.visit(bot, user2, false)
      %Subscription{updated_at: t!} = Subscription.get(user2, bot)
      assert_push "subscription:data", push, 2000
      assert push == expected.(1, "ARRIVE", t!)

      Bots.depart(bot, user2, false)
      %Subscription{updated_at: t!} = Subscription.get(user2, bot)
      assert_push "subscription:data", push, 2000
      assert push == expected.(0, "DEPART", t!)
    end

    test "unauthenticated user attempting subscription", %{socket: socket} do
      socket
      |> push_doc(@subscription)
      |> assert_unauthenticated_reply()
    end
  end

  describe "client test analogs" do
    test "botGuestVisitors subscription", %{
      socket: socket,
      token: token,
      user: %{id: user_id} = user
    } do
      :os.putenv('WOCKY_ENTER_DEBOUNCE_SECONDS', '0')
      bot = Factory.insert(:bot, user: user)
      Factory.insert(:subscription, bot: bot, user: user)

      authenticate(user_id, token, socket)

      sub = """
      subscription {
        botGuestVisitors {
          bot {
            id
          }
          visitor {
            id
          }
          action
        }
      }
      """

      ref! = push_doc(socket, sub)
      assert_reply ref!, :ok, %{subscriptionId: _subscription_id}, 1000

      mut = """
      mutation ($input: UserLocationUpdateInput!) {
        userLocationUpdate (input: $input) {
          successful
        }
      }
      """

      {lat, lon} = GeoUtils.get_lat_lon(bot.location)

      location_input = %{
        "input" => %{
          "lat" => lat,
          "lon" => lon,
          "accuracy" => 1.0,
          "device" => Factory.device(),
          "isFetch" => true
        }
      }

      ref! = push_doc(socket, mut, variables: location_input)
      assert_reply ref!, :ok, _, 1000
      ref! = push_doc(socket, mut, variables: location_input)
      assert_reply ref!, :ok, _, 1000

      assert_push "subscription:data", _push, 2000
    end
  end

  describe "contacts subscription" do
    setup do
      user2 = Factory.insert(:user)
      {:ok, user2: user2}
    end

    @subscription """
    subscription {
      contacts {
        user {
          id
        }
        relationship
        created_at
      }
    }
    """

    test "should notify when a new contact is created", %{
      socket: socket,
      token: token,
      user: user,
      user2: user2 = %{id: user2_id}
    } do
      authenticate(user.id, token, socket)

      ref = push_doc(socket, @subscription)
      assert_reply ref, :ok, %{subscriptionId: subscription_id}, 1000

      Roster.befriend(user, user2)

      assert_push "subscription:data", push, 1000

      assert %{
               result: %{
                 data: %{
                   "contacts" => %{
                     "relationship" => "FRIEND",
                     "created_at" => _,
                     "user" => %{"id" => ^user2_id}
                   }
                 }
               },
               subscriptionId: ^subscription_id
             } = push
    end

    test "should notify when a contact type is changed", %{
      socket: socket,
      token: token,
      user: user,
      user2: user2
    } do
      authenticate(user.id, token, socket)

      ref = push_doc(socket, @subscription)
      assert_reply ref, :ok, %{subscriptionId: subscription_id}, 1000

      Roster.befriend(user, user2)
      assert_relationship_notification("FRIEND", user2, subscription_id)

      Roster.unfriend(user, user2)
      assert_relationship_notification("NONE", user2, subscription_id)
    end
  end

  defp assert_unauthenticated_reply(ref) do
    assert_reply ref,
                 :error,
                 %{
                   errors: [
                     %{
                       message: "This operation requires an authenticated user"
                     }
                   ]
                 },
                 1000
  end

  defp assert_relationship_notification(relationship, user, subscription_id) do
    assert_push "subscription:data", push, 1000
    id = user.id

    assert %{
             result: %{
               data: %{
                 "contacts" => %{
                   "relationship" => ^relationship,
                   "created_at" => _,
                   "user" => %{"id" => ^id}
                 }
               }
             },
             subscriptionId: ^subscription_id
           } = push
  end
end
