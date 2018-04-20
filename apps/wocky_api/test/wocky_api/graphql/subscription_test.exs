defmodule WockyAPI.GraphQL.SubscriptionTest do
  use WockyAPI.SubscriptionCase, async: false

  alias Wocky.Bot
  alias Wocky.Repo
  alias Wocky.Repo.Factory
  alias Wocky.Bot.Subscription
  alias Wocky.User
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

    @authenticate """
    mutation ($input: AuthenticateInput) {
      authenticate(input: $input) {
        user {
          id
        }
      }
    }
    """

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

      ref =
        push_doc(
          socket,
          @authenticate,
          variables: %{input: %{user: user_id, token: token}}
        )

      assert_reply ref,
                   :ok,
                   %{
                     data: %{"authenticate" => %{"user" => %{"id" => ^user_id}}}
                   },
                   1000

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
      ref = push_doc(socket, @subscription)
      assert_reply ref,
        :error,
        %{
          errors: [%{
            message: "This operation requires an authenticated user"
          }]
        },
        1000
    end
  end
end
