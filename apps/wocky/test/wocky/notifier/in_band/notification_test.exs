defmodule Wocky.Notifier.InBand.NotificationTest do
  use Wocky.DataCase, async: true

  alias Wocky.Block
  alias Wocky.Events.BotInvitation
  alias Wocky.Events.BotInvitationResponse
  alias Wocky.Events.BotItem
  alias Wocky.Events.GeofenceEvent
  alias Wocky.Events.LocationShare
  alias Wocky.Events.LocationShareEnd
  alias Wocky.Events.UserInvitation
  alias Wocky.Notifier
  alias Wocky.Notifier.InBand.Notification
  alias Wocky.Relations.Invitation
  alias Wocky.Repo.Factory

  setup do
    [user, user2] = Factory.insert_list(2, :user)
    bot = Factory.insert(:bot, user: user)

    bot_invitation =
      Factory.insert(:bot_invitation, user: user, invitee: user2, bot: bot)

    {:ok, user: user, user2: user2, bot: bot, bot_invitation: bot_invitation}
  end

  describe "create notification" do
    test "bot invitation", ctx do
      Notifier.notify(%BotInvitation{
        to: ctx.user2,
        from: ctx.user,
        bot: ctx.bot,
        invitation: ctx.bot_invitation
      })

      assert %Notification{type: :bot_invitation} =
               Repo.get_by(Notification, user_id: ctx.user2.id)
    end

    test "bot invitation response", ctx do
      invitation = %Invitation{ctx.bot_invitation | accepted: true}

      Notifier.notify(%BotInvitationResponse{
        to: ctx.user,
        from: ctx.user2,
        bot: ctx.bot,
        invitation: invitation
      })

      assert %Notification{type: :bot_invitation_response} =
               Repo.get_by(Notification, user_id: ctx.user.id)
    end

    test "bot item", ctx do
      item = Factory.insert(:item, bot: ctx.bot, user: ctx.user2)

      Notifier.notify(%BotItem{
        to: ctx.user,
        from: ctx.user2,
        item: item
      })

      assert %Notification{type: :bot_item} =
               Repo.get_by(Notification, user_id: ctx.user.id)
    end

    test "geofence event", ctx do
      Notifier.notify(%GeofenceEvent{
        to: ctx.user,
        from: ctx.user2,
        bot: ctx.bot,
        event: :enter
      })

      assert %Notification{type: :geofence_event} =
               Repo.get_by(Notification, user_id: ctx.user.id)
    end

    test "location share", ctx do
      Notifier.notify(%LocationShare{
        to: ctx.user,
        from: ctx.user2,
        expires_at: DateTime.utc_now()
      })

      assert %Notification{type: :location_share} =
               Repo.get_by(Notification, user_id: ctx.user.id)
    end

    test "location share end", ctx do
      Notifier.notify(%LocationShareEnd{
        to: ctx.user,
        from: ctx.user2
      })

      assert %Notification{type: :location_share_end} =
               Repo.get_by(Notification, user_id: ctx.user.id)
    end

    test "user invitation", ctx do
      Notifier.notify(%UserInvitation{
        to: ctx.user,
        from: ctx.user2
      })

      assert %Notification{type: :user_invitation} =
               Repo.get_by(Notification, user_id: ctx.user.id)
    end
  end

  describe "blocked user notification should fail" do
    setup ctx do
      Block.block(ctx.user, ctx.user2)
      :ok
    end

    test "bot invitation", ctx do
      Notifier.notify(%BotInvitation{
        to: ctx.user2,
        from: ctx.user,
        bot: ctx.bot,
        invitation: ctx.bot_invitation
      })

      refute Repo.get_by(Notification, user_id: ctx.user2.id)
    end

    test "bot invitation response", ctx do
      invitation = %Invitation{ctx.bot_invitation | accepted: true}

      Notifier.notify(%BotInvitationResponse{
        to: ctx.user,
        from: ctx.user2,
        bot: ctx.bot,
        invitation: invitation
      })

      refute Repo.get_by(Notification, user_id: ctx.user.id)
    end

    test "bot item", ctx do
      item = Factory.insert(:item, bot: ctx.bot, user: ctx.user2)

      Notifier.notify(%BotItem{
        to: ctx.user,
        from: ctx.user2,
        item: item
      })

      refute Repo.get_by(Notification, user_id: ctx.user.id)
    end

    test "geofence event", ctx do
      Notifier.notify(%GeofenceEvent{
        to: ctx.user,
        from: ctx.user2,
        bot: ctx.bot,
        event: :enter
      })

      refute Repo.get_by(Notification, user_id: ctx.user.id)
    end

    test "location_share", ctx do
      Notifier.notify(%LocationShare{
        to: ctx.user,
        from: ctx.user2,
        expires_at: DateTime.utc_now()
      })

      refute Repo.get_by(Notification, user_id: ctx.user.id)
    end

    test "location_share_end", ctx do
      Notifier.notify(%LocationShareEnd{
        to: ctx.user,
        from: ctx.user2
      })

      refute Repo.get_by(Notification, user_id: ctx.user.id)
    end

    test "user invitation", ctx do
      Notifier.notify(%UserInvitation{
        to: ctx.user,
        from: ctx.user2
      })

      refute Repo.get_by(Notification, user_id: ctx.user.id)
    end
  end

  describe "delete/2" do
    setup %{user: user, user2: user2} do
      user3 = Factory.insert(:user)

      notification =
        Factory.insert(:bot_invitation_notification,
          user: user,
          other_user: user2
        )

      notification2 =
        Factory.insert(:bot_invitation_notification,
          user: user,
          other_user: user3
        )

      Notification.delete(user, user2)

      {:ok, notification: notification, notification2: notification2}
    end

    test "it should delete the notification between the users", ctx do
      refute Repo.get(Notification, ctx.notification.id)
    end

    test "it should not delete other notifications to the user", ctx do
      assert Repo.get(Notification, ctx.notification2.id)
    end
  end

  describe "user_query/3" do
    setup ctx do
      n = Factory.insert_list(5, :geofence_event_notification, user: ctx.user)
      {:ok, notifications: Enum.reverse(n)}
    end

    test "get all notifications", ctx do
      assert ids_match(
               ctx.user
               |> Notification.user_query(nil, nil)
               |> order_by(desc: :updated_at)
               |> Repo.all(),
               ctx.notifications
             )
    end

    test "get head notifications", ctx do
      assert ids_match(
               ctx.user
               |> Notification.user_query(nil, List.last(ctx.notifications).id)
               |> order_by(desc: :updated_at)
               |> Repo.all(),
               Enum.slice(ctx.notifications, 0..3)
             )
    end

    test "get tail notifications", ctx do
      assert ids_match(
               ctx.user
               |> Notification.user_query(hd(ctx.notifications).id, nil)
               |> order_by(desc: :updated_at)
               |> Repo.all(),
               tl(ctx.notifications)
             )
    end

    test "get middle notifications", ctx do
      assert ids_match(
               ctx.user
               |> Notification.user_query(
                 hd(ctx.notifications).id,
                 List.last(ctx.notifications).id
               )
               |> order_by(desc: :updated_at)
               |> Repo.all(),
               Enum.slice(ctx.notifications, 1..3)
             )
    end

    test "filter on notification type", ctx do
      assert ids_match(
               ctx.user
               |> Notification.user_query(
                 hd(ctx.notifications).id,
                 nil,
                 [:bot_item]
               )
               |> order_by(desc: :updated_at)
               |> Repo.all(),
               []
             )

      assert ids_match(
               ctx.user
               |> Notification.user_query(
                 hd(ctx.notifications).id,
                 nil,
                 [:geofence_event]
               )
               |> order_by(desc: :updated_at)
               |> Repo.all(),
               Enum.slice(ctx.notifications, 1..4)
             )
    end

    defp ids_match(a, b), do: Enum.map(a, & &1.id) == Enum.map(b, & &1.id)
  end
end
