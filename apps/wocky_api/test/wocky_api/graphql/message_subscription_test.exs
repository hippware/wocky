defmodule WockyAPI.GraphQL.MessageSubscriptionTest do
  use WockyAPI.SubscriptionCase, async: false

  import WockyAPI.ChannelHelper

  alias Wocky.Repo.Factory

  setup_all do
    setup_watcher()
  end

  describe "messages subscription" do
    @subscription """
    subscription {
      messages {
        other_user { id }
        message
        direction
      }
    }
    """

    setup %{socket: socket, token: token, user: %{id: user_id}} do
      authenticate(user_id, token, socket)

      ref = push_doc(socket, @subscription)
      assert_reply ref, :ok, %{subscriptionId: subscription_id}, 1000
      {:ok, subscription_id: subscription_id}
    end

    test "should notify when a new message is received", %{
      user: user,
      subscription_id: subscription_id
    } do
      m = Factory.insert(:message, recipient: user)

      assert_push "subscription:data", push, 1000

      assert %{
               result: %{
                 data: %{
                   "messages" => %{
                     "other_user" => %{
                       "id" => m.sender.id
                     },
                     "message" => m.message,
                     "direction" => "INCOMING"
                   }
                 }
               },
               subscriptionId: subscription_id
             } == push
    end

    test "should not notify the sender when a new message is sent", %{
      user: user
    } do
      Factory.insert(:message, sender: user)

      refute_push "subscription:data", _push, 500
    end
  end
end
