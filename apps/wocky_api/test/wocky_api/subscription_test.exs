defmodule WockyAPI.SubscriptionTest do
  use WockyAPI.SubscriptionCase, async: false

  alias Wocky.Bot
  alias Wocky.Repo
  alias Wocky.Repo.Factory
  alias Wocky.Bot.Subscription
  alias Wocky.User
  alias WockyAPI.Callbacks

  setup_all do
    Callbacks.register()
    Ecto.Adapters.SQL.Sandbox.mode(Wocky.Repo, :auto)
    Application.start(:wocky_db_watcher)

    on_exit fn ->
      Application.stop(:wocky_db_watcher)
      Repo.delete_all(User)
    end
  end

  describe "watch for visitor count change" do
    setup do
      user2 = Factory.insert(:user)
      bot = Factory.insert(:bot, public: true)
      Subscription.put(user2, bot)

      on_exit(fn ->
        Application.stop(:wocky_db_watcher)
        Repo.delete_all(User)
      end)

      {:ok, user2: user2, bot: bot}
    end

    @authenticate """
    mutation ($user: String!, $token: String!) {
      authenticate(user: $user, token: $token) {
        id
      }
    }
    """

    @subscription """
    subscription ($id: String!) {
      botVisitors (id: $id) {
        subscribers (first: 0, type: VISITOR) {
          totalCount
        }
      }
    }
    """
    test "visitor count changes",
    %{socket: socket, user2: user2, bot: bot, user: %{id: user_id},
      token: token} do
      ref = push_doc(socket, @authenticate,
                     variables: %{user: user_id, token: token})
      assert_reply ref, :ok, %{data: %{"authenticate" => %{"id" => ^user_id}}}, 1000

      ref = push_doc(socket, @subscription, variables: %{id: bot.id})
      assert_reply ref, :ok, %{subscriptionId: subscription_id}, 1000

      expected = fn(count) ->
        %{
          result: %{
            data:
            %{"botVisitors" => %{"subscribers" => %{"totalCount" => count}}}},
          subscriptionId: subscription_id
        }
      end

      Bot.visit(bot, user2)
      assert_push "subscription:data", push, 1000
      assert push == expected.(1)

      Bot.depart(bot, user2)
      assert_push "subscription:data", push, 1000
      assert push == expected.(0)
    end
  end

end
