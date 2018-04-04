defmodule WockyAPI.SubscriptionTest do
  use WockyAPI.SubscriptionCase

  alias Wocky.Bot
  alias Wocky.Repo.Factory
  alias Wocky.Bot.Subscription
  alias Wocky.Watcher.Client

  describe "watch for visitor count change" do
    setup do
      Ecto.Adapters.SQL.Sandbox.mode(Wocky.Repo, :auto)
      Application.start(:wocky_db_watcher)
      Client.start_link()

      user2 = Factory.insert(:user)
      bot = Factory.insert(:bot, public: true)
      Subscription.put(user2, bot)
      {:ok, user2: user2, bot: bot}
    end

    @subscription """
    subscription ($id: String!) {
      botVisitors (id: $id) {
        subscribers (first: 0, type: VISITOR) {
          totalCount
        }
      }
    }
    """

    test "visitor count changes", %{socket: socket, user2: user2, bot: bot} do
      ref = push_doc(socket, @subscription, variables: %{id: bot.id})
      assert_reply ref, :ok, %{subscriptionId: subscription_id}, 500

      expected = fn(count) ->
        %{
          result: %{
            data:
            %{"botVisitors" => %{"subscribers" => %{"totalCount" => count}}}},
          subscriptionId: subscription_id
        }
      end

      Bot.visit(bot, user2)
      assert_push "subscription:data", push, 500
      assert push == expected.(1)

      Bot.depart(bot, user2)
      assert_push "subscription:data", push, 500
      assert push == expected.(0)
    end
  end

end
