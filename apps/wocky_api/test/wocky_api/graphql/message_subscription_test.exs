defmodule WockyAPI.GraphQL.MessageSubscriptionTest do
  use WockyAPI.SubscriptionCase, async: false

  import WockyAPI.ChannelHelper

  alias Wocky.Repo.Factory

  setup_all :require_watcher

  describe "messages subscription" do
    @subscription """
    subscription {
      messages {
        other_user { id }
        content
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
      sender_id = m.sender.id
      content = m.content

      assert_subscription_update %{
        result: %{
          data: %{
            "messages" => %{
              "other_user" => %{
                "id" => ^sender_id
              },
              "content" => ^content,
              "direction" => "INCOMING"
            }
          }
        },
        subscriptionId: ^subscription_id
      }
    end

    test "should not notify the sender when a new message is sent", %{
      user: user
    } do
      Factory.insert(:message, sender: user)

      refute_subscription_update _data
    end
  end
end
