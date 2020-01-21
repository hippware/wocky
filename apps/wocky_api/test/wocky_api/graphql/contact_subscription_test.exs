defmodule WockyAPI.GraphQL.ContactSubscriptionTest do
  use WockyAPI.SubscriptionCase, async: false

  alias Wocky.Account
  alias Wocky.Contacts

  setup_all do
    require_watcher()

    WockyAPI.Callbacks.Relationship.register()
    WockyAPI.Callbacks.User.register()
  end

  setup %{socket: socket, token: token, user: %{id: user_id}} do
    authenticate(user_id, token, socket)

    friend = Factory.insert(:user)

    {:ok, friend: friend}
  end

  describe "contacts subscription" do
    @query """
    subscription {
      contacts {
        user { id }
        relationship
        share_type
      }
    }
    """

    setup ctx do
      ref = push_doc(ctx.socket, @query)
      assert_reply ref, :ok, %{subscriptionId: subscription_id}, 150

      {:ok, subscription_id: subscription_id}
    end

    test "should send a message when a user becomes a friend", %{
      user: user,
      friend: %{id: friend_id} = friend,
      subscription_id: subscription_id
    } do
      Contacts.befriend(user, friend, :always)

      assert_subscription_update %{
        result: %{
          data: %{
            "contacts" => %{
              "user" => %{
                "id" => ^friend_id
              },
              "relationship" => "FRIEND",
              "share_type" => "ALWAYS"
            }
          }
        },
        subscriptionId: ^subscription_id
      }
    end

    test "should send a message when a friendship ends", %{
      user: user,
      friend: %{id: friend_id} = friend,
      subscription_id: subscription_id
    } do
      Contacts.befriend(user, friend, :always)
      Contacts.unfriend(user, friend)

      assert_subscription_update %{
        result: %{
          data: %{
            "contacts" => %{
              "user" => %{
                "id" => ^friend_id
              },
              "relationship" => "NONE",
              "share_type" => "DISABLED"
            }
          }
        },
        subscriptionId: ^subscription_id
      }
    end
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

    setup %{user: user, friend: friend, socket: socket} do
      stranger = Factory.insert(:user)

      Contacts.befriend(friend, user)

      ref = push_doc(socket, @query)
      assert_reply ref, :ok, %{subscriptionId: subscription_id}, 150

      {:ok, stranger: stranger, subscription_id: subscription_id}
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
end
