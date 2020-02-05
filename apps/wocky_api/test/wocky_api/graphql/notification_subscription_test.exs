defmodule WockyAPI.GraphQL.NotificationSubscriptionTest do
  use WockyAPI.SubscriptionCase, async: false

  import Eventually

  alias Wocky.Contacts
  alias Wocky.Contacts.Share.Cache
  alias Wocky.Location
  alias Wocky.Location.Handler
  alias Wocky.Presence.Manager
  alias Wocky.Relation
  alias Wocky.Repo
  alias WockyAPI.Factory, as: APIFactory

  setup_all do
    require_watcher()

    Wocky.Callbacks.register()
    WockyAPI.Callbacks.register()
  end

  @subscription """
  subscription {
    notifications {
      __typename
      ... on Notification {
        data {
          __typename
          ... on BotInvitationNotification {
            invitation { id }
            user { id }
            bot { id }
          }
          ... on BotInvitationResponseNotification {
            invitation { id }
            user { id }
            bot { id }
            accepted
          }
          ... on BotItemNotification {
            user { id }
            bot { id }
            bot_item { id }
          }
          ... on GeofenceEventNotification {
            user { id }
            bot { id }
            event
          }
          ... on LocationShareNotification {
            user { id }
            shareId
            expiresAt
            shareTypes {
              to
              from
            }
          }
          ... on LocationShareEndNotification {
            shareId
            user { id }
          }
          ... on LocationShareEndSelfNotification {
            shareId
            user { id }
          }
          ... on LocationShareNearbyStartNotification {
            user { id }
          }
          ... on UserBefriendNotification {
            user { id }
          }
          ... on UserInvitationNotification {
            user { id }
          }
        }
        createdAt
      }
      ... on NotificationDeleted {
        id
      }
    }
  }
  """
  setup %{user: user, socket: socket, token: token} do
    authenticate(user.id, token, socket)
    ref = push_doc(socket, @subscription)
    assert_reply ref, :ok, %{subscriptionId: subscription_id}, 150

    user2 = Factory.insert(:user)
    bot = Factory.insert(:bot, user: user)
    Relation.subscribe(user2, bot)

    assert_eventually(Relation.subscribed?(user, bot))

    on_exit(fn ->
      Wocky.Repo.delete_all(Wocky.Account.User)
      Handler.stop_all()
      Manager.stop_all()
    end)

    {:ok, user2: user2, bot: bot, ref: ref, subscription_id: subscription_id}
  end

  describe "event notifications" do
    test "user posts item to bot", %{
      user2: user2,
      bot: bot,
      subscription_id: subscription_id
    } do
      item = Factory.insert(:item, user: user2, bot: bot)

      assert_notification_update(subscription_id, %{
        "__typename" => "BotItemNotification",
        "bot" => %{"id" => bot.id},
        "bot_item" => %{"id" => item.id},
        "user" => %{"id" => user2.id}
      })
    end

    test "user enters bot", %{
      user: user,
      user2: user2,
      bot: bot,
      subscription_id: subscription_id
    } do
      befriend(user, user2, subscription_id)

      Relation.visit(user2, bot, true)

      assert_notification_update(subscription_id, %{
        "__typename" => "GeofenceEventNotification",
        "bot" => %{"id" => bot.id},
        "user" => %{"id" => user2.id},
        "event" => "ENTER"
      })
    end

    test "user exits bot", %{
      user: user,
      user2: user2,
      bot: bot,
      subscription_id: subscription_id
    } do
      befriend(user, user2, subscription_id)

      Relation.depart(user2, bot, true)

      assert_notification_update(subscription_id, %{
        "__typename" => "GeofenceEventNotification",
        "bot" => %{"id" => bot.id},
        "user" => %{"id" => user2.id},
        "event" => "EXIT"
      })
    end

    test "user receives an invitation", %{
      user: user,
      user2: user2,
      subscription_id: subscription_id
    } do
      bot2 = Factory.insert(:bot, user: user2)
      befriend(user, user2, subscription_id)
      {:ok, invitation} = Relation.invite(user, bot2, user2)

      assert_notification_update(subscription_id, %{
        "__typename" => "BotInvitationNotification",
        "invitation" => %{"id" => to_string(invitation.id)},
        "bot" => %{"id" => bot2.id},
        "user" => %{"id" => user2.id}
      })
    end

    test "user responds to an invitation", %{
      user: user,
      user2: user2,
      bot: bot,
      subscription_id: subscription_id
    } do
      invitation =
        Factory.insert(:bot_invitation, user: user, invitee: user2, bot: bot)

      Relation.respond(invitation, true, user2)

      assert_notification_update(subscription_id, %{
        "__typename" => "BotInvitationResponseNotification",
        "invitation" => %{"id" => to_string(invitation.id)},
        "bot" => %{"id" => bot.id},
        "user" => %{"id" => user2.id},
        "accepted" => true
      })
    end

    test "user invites", %{
      user: user,
      user2: user2,
      subscription_id: subscription_id
    } do
      Contacts.make_friends(user2, user, :disabled)

      assert_notification_update(subscription_id, %{
        "__typename" => "UserInvitationNotification",
        "user" => %{"id" => user2.id}
      })
    end

    defp assert_location_update(subscription_id, user_id, share_id) do
      assert_subscription_update %{
        result: %{
          data: %{
            "notifications" => %{
              "__typename" => "Notification",
              "createdAt" => _,
              "data" => %{
                "__typename" => "LocationShareNotification",
                "user" => %{"id" => ^user_id},
                "expiresAt" => _,
                "shareId" => ^share_id,
                "shareTypes" => %{
                  "to" => "DISABLED",
                  "from" => "ALWAYS"
                }
              }
            }
          }
        },
        subscriptionId: ^subscription_id
      }
    end

    test "user shares their location", %{
      user: user,
      user2: user2,
      subscription_id: subscription_id
    } do
      befriend(user, user2, subscription_id)

      {:ok, %{share_id: id}} = Contacts.update_sharing(user2, user, :always)

      assert_location_update(subscription_id, user2.id, to_string(id))
    end

    test "other user stops sharing their location", %{
      user: user,
      user2: user2,
      subscription_id: subscription_id
    } do
      befriend(user, user2, subscription_id)

      {:ok, %{share_id: id}} = Contacts.update_sharing(user2, user, :always)

      assert_location_update(subscription_id, user2.id, to_string(id))

      Contacts.update_sharing(user2, user, :disabled)

      assert_notification_update(subscription_id, %{
        "__typename" => "LocationShareEndNotification",
        "user" => %{"id" => user2.id},
        "shareId" => to_string(id)
      })
    end

    test "moves into nearby range", %{
      user: user,
      user2: user2,
      subscription_id: subscription_id
    } do
      befriend(user, user2, subscription_id, :nearby)
      assert_eventually(length(Cache.get(user2.id)) == 1)
      assert hd(Cache.get(user2.id)).share_type == :nearby

      now = DateTime.utc_now()

      location =
        Factory.build(:location,
          lat: 0.0,
          lon: 0.0,
          captured_at: now
        )

      {:ok, _} =
        Location.set_user_location(user, %{location | user_id: user.id})

      {:ok, _} =
        Location.set_user_location(user2, %{location | user_id: user2.id})

      assert_notification_update(subscription_id, %{
        "__typename" => "LocationShareNearbyStartNotification",
        "user" => %{"id" => user2.id}
      })
    end

    test """
         moves into nearby range should only generate one notifiation in the
         cooldown period
         """,
         %{
           user: user,
           user2: user2,
           subscription_id: subscription_id
         } do
      befriend(user, user2, subscription_id, :nearby)
      assert_eventually(length(Cache.get(user2.id)) == 1)
      assert hd(Cache.get(user2.id)).share_type == :nearby

      now = DateTime.utc_now()

      location =
        Factory.build(:location,
          user_id: user.id,
          lat: 0.0,
          lon: 0.0,
          captured_at: now
        )

      location2 = %{location | user_id: user2.id}

      {:ok, _} = Location.set_user_location(user, location)

      {:ok, _} = Location.set_user_location(user2, location2)

      assert_notification_update(subscription_id, %{
        "__typename" => "LocationShareNearbyStartNotification",
        "user" => %{"id" => user2.id}
      })

      {:ok, _} = Location.set_user_location(user2, location2)

      refute_subscription_update _data
    end

    test """
         moves into nearby range should generate a new notification once the
         cooldown period has expired
         """,
         %{
           user: user,
           user2: user2,
           subscription_id: subscription_id
         } do
      befriend(user, user2, subscription_id)

      {:ok, _} =
        Contacts.update_sharing(user2, user, :nearby, nearby_cooldown: 1)

      assert_eventually(length(Cache.get(user2.id)) == 1)
      assert hd(Cache.get(user2.id)).share_type == :nearby

      now = DateTime.utc_now()

      location =
        Factory.build(:location,
          user_id: user.id,
          lat: 0.0,
          lon: 0.0,
          captured_at: now
        )

      location2 = %{location | user_id: user2.id}

      {:ok, _} = Location.set_user_location(user, location)
      {:ok, _} = Location.set_user_location(user2, location2)

      assert_notification_update(subscription_id, %{
        "__typename" => "LocationShareNearbyStartNotification",
        "user" => %{"id" => user2.id}
      })

      {:ok, _} = Location.set_user_location(user2, location2)

      assert_notification_update(subscription_id, %{
        "__typename" => "LocationShareNearbyStartNotification",
        "user" => %{"id" => user2.id}
      })
    end

    test "user stops sharing their own location", %{
      user: user,
      user2: user2,
      subscription_id: subscription_id
    } do
      befriend(user, user2, subscription_id)

      {:ok, %{share_id: id}} = Contacts.update_sharing(user, user2, :always)

      Contacts.update_sharing(user, user2, :disabled)

      assert_notification_update(subscription_id, %{
        "__typename" => "LocationShareEndSelfNotification",
        "user" => %{"id" => user2.id},
        "shareId" => to_string(id)
      })
    end
  end

  describe "notification deletion" do
    setup %{user: user} do
      notification = APIFactory.insert(:bot_item_notification, user: user)
      assert_subscription_update _data

      {:ok, notification: notification}
    end

    test "notification deleted", %{
      notification: notification,
      subscription_id: subscription_id
    } do
      Repo.delete(notification)

      id_str = to_string(notification.id)

      assert_subscription_update %{
        result: %{
          data: %{
            "notifications" => %{
              "__typename" => "NotificationDeleted",
              "id" => ^id_str
            }
          }
        },
        subscriptionId: ^subscription_id
      }
    end
  end

  defp assert_notification_update(subscription_id, data) do
    assert_subscription_update %{
      result: %{
        data: %{
          "notifications" => %{
            "__typename" => "Notification",
            "createdAt" => _,
            "data" => ^data
          }
        }
      },
      subscriptionId: ^subscription_id
    }
  end

  defp befriend(user, user2, subscription_id, share_type \\ :disabled) do
    Contacts.befriend(user, user2, share_type)

    assert_notification_update(subscription_id, %{
      "__typename" => "UserBefriendNotification",
      "user" => %{"id" => user2.id}
    })
  end
end
