defmodule WockyAPI.GraphQL.SubscriptionTest do
  use WockyAPI.SubscriptionCase, async: false

  import WockyAPI.ChannelHelper

  alias Faker.Lorem
  alias Wocky.{Bot, HomeStream, Repo, Roster, User}
  alias Wocky.Bot.Subscription
  alias Wocky.HomeStream
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

  @subscription """
  subscription {
    discoverBots {
      bot {
        id
      }
      action
    }
  }
  """
  describe "discovered bots list" do
    setup %{user: user} do
      friend = Factory.insert(:user)
      Roster.befriend(friend.id, user.id)
      {:ok, friend: friend}
    end

    test "get updates", %{
      socket: socket,
      friend: friend,
      token: token,
      user: %{id: user_id}
    } do
      authenticate(user_id, token, socket)

      ref = push_doc(socket, @subscription)
      assert_reply ref, :ok, %{subscriptionId: subscription_id}, 1000

      expected = fn id, action ->
        %{
          result: %{
            data: %{
              "discoverBots" => %{
                "action" => action,
                "bot" => %{"id" => id}
              }
            }
          },
          subscriptionId: subscription_id
        }
      end

      bot = Factory.insert(:bot, user: friend, public: true)
      assert_push "subscription:data", push, 2000
      assert push == expected.(bot.id, "CREATED")

      private_bot = Factory.insert(:bot, user: friend)
      refute_push "subscription:data", _push

      Bot.update(bot, %{public: false})
      assert_push "subscription:data", push, 2000
      assert push == expected.(bot.id, "PRIVATIZED")

      Bot.update(private_bot, %{public: true})
      assert_push "subscription:data", push, 2000
      assert push == expected.(private_bot.id, "PUBLICIZED")

      Bot.delete(private_bot)
      assert_push "subscription:data", push, 2000
      assert push == expected.(private_bot.id, "DELETED")
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
