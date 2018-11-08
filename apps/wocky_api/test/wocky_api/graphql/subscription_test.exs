defmodule WockyAPI.GraphQL.SubscriptionTest do
  use WockyAPI.SubscriptionCase, async: false

  import WockyAPI.ChannelHelper

  alias Faker.Lorem
  alias Wocky.{Bot, GeoUtils, HomeStream, Repo, Roster, User}
  alias Wocky.Bot.Subscription
  alias Wocky.Repo.Factory
  alias Wocky.Watcher.Client
  alias WockyAPI.Callbacks

  setup_all do
    Client.clear_all_subscriptions()
    Callbacks.register()
    Ecto.Adapters.SQL.Sandbox.mode(Wocky.Repo, :auto)
    Application.start(:wocky_db_watcher)

    on_exit(fn ->
      Application.stop(:wocky_db_watcher)
      Repo.delete_all(User)
    end)
  end

  describe "watch for visitor count change" do
    setup do
      user2 = Factory.insert(:user)
      bot = Factory.insert(:bot, public: true)
      Subscription.put(user2, bot)

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
      Bot.subscribe(bot, user, true)

      authenticate(user_id, token, socket)

      ref = push_doc(socket, @subscription)
      assert_reply ref, :ok, %{subscriptionId: subscription_id}, 1000

      expected = fn count, action ->
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
                "action" => action
              }
            }
          },
          subscriptionId: subscription_id
        }
      end

      Bot.visit(bot, user2)
      assert_push "subscription:data", push, 2000
      assert push == expected.(1, "ARRIVE")

      Bot.depart(bot, user2)
      assert_push "subscription:data", push, 2000
      assert push == expected.(0, "DEPART")
    end

    test "unauthenticated user attempting subscription", %{socket: socket} do
      socket
      |> push_doc(@subscription)
      |> assert_unauthenticated_reply()
    end
  end

  @subscription """
  subscription {
    homeStream {
      action
      item {
        key
        from_jid
        stanza
        user {
          id
        }
        referenceBot {
          id
        }
        referenceUser {
          id
        }
      }
    }
  }
  """
  describe "watch home stream for changes" do
    setup do
      user2 = Factory.insert(:user)
      bot = Factory.insert(:bot, public: true)
      {:ok, user2: user2, bot: bot}
    end

    test "Home Stream item updates", %{
      socket: socket,
      user2: user2,
      bot: bot,
      user: %{id: user_id},
      token: token
    } do
      authenticate(user_id, token, socket)

      ref = push_doc(socket, @subscription)
      assert_reply ref, :ok, %{subscriptionId: subscription_id}, 1000

      key = Lorem.word()
      from_jid = Factory.new_jid()
      stanza = Lorem.paragraph()

      expected = fn action ->
        %{
          result: %{
            data: %{
              "homeStream" => %{
                "action" => action,
                "item" => %{
                  "key" => key,
                  "from_jid" => from_jid,
                  "stanza" => stanza,
                  "user" => %{"id" => user_id},
                  "referenceBot" => %{"id" => bot.id},
                  "referenceUser" => %{"id" => user2.id}
                }
              }
            }
          },
          subscriptionId: subscription_id
        }
      end

      HomeStream.put(
        user_id,
        key,
        from_jid,
        stanza,
        ref_bot_id: bot.id,
        ref_user_id: user2.id
      )

      assert_push "subscription:data", push, 2000
      assert push == expected.("INSERT")

      HomeStream.put(
        user_id,
        key,
        from_jid,
        stanza,
        ref_bot_id: bot.id,
        ref_user_id: user2.id
      )

      assert_push "subscription:data", push, 2000
      assert push == expected.("UPDATE")

      HomeStream.delete(user_id, key)
      assert_push "subscription:data", push, 2000

      assert push ==
               %{
                 result: %{
                   data: %{
                     "homeStream" => %{
                       "action" => "DELETE",
                       "item" => %{
                         "key" => key,
                         "from_jid" => "",
                         "stanza" => "",
                         "user" => %{"id" => user_id},
                         "referenceBot" => nil,
                         "referenceUser" => nil
                       }
                     }
                   }
                 },
                 subscriptionId: subscription_id
               }
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
      bot = Factory.insert(:bot, user: user, geofence: true, public: true)
      Bot.subscribe(bot, user, true)

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

      ref = push_doc(socket, sub)
      assert_reply ref, :ok, %{subscriptionId: _subscription_id}, 1000

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
          "resource" => Faker.String.base64(),
          "isFetch" => true
        }
      }

      ref = push_doc(socket, mut, variables: location_input)
      assert_reply ref, :ok, _, 1000
      ref = push_doc(socket, mut, variables: location_input)
      assert_reply ref, :ok, _, 1000

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
      user: %{id: user_id},
      user2: %{id: user2_id}
    } do
      authenticate(user_id, token, socket)

      ref = push_doc(socket, @subscription)
      assert_reply ref, :ok, %{subscriptionId: subscription_id}, 1000

      Roster.befriend(user_id, user2_id)

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
      user: %{id: user_id},
      user2: %{id: user2_id}
    } do
      Roster.follow(user2_id, user_id)

      authenticate(user_id, token, socket)

      ref = push_doc(socket, @subscription)
      assert_reply ref, :ok, %{subscriptionId: subscription_id}, 1000

      Roster.befriend(user_id, user2_id)

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

    test "should notify when a contact is removed", %{
      socket: socket,
      token: token,
      user: %{id: user_id},
      user2: %{id: user2_id}
    } do
      Roster.befriend(user2_id, user_id)

      authenticate(user_id, token, socket)

      ref = push_doc(socket, @subscription)
      assert_reply ref, :ok, %{subscriptionId: subscription_id}, 1000

      Roster.unfriend(user_id, user2_id)

      assert_push "subscription:data", push, 1000

      assert %{
               result: %{
                 data: %{
                   "contacts" => %{
                     "relationship" => "NONE",
                     "created_at" => _,
                     "user" => %{"id" => ^user2_id}
                   }
                 }
               },
               subscriptionId: ^subscription_id
             } = push
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
end
