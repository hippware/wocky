defmodule Wocky.User.NotificationTest do
  use Wocky.DataCase

  alias Wocky.Block
  alias Wocky.Repo.Factory
  alias Wocky.User.Notification

  alias Wocky.User.Notification.{
    BotInvitation,
    BotInvitationResponse,
    BotItem,
    GeofenceEvent,
    UserInvitation
  }


  setup do
    [user, user2] = Factory.insert_list(2, :user)
    bot = Factory.insert(:bot, user: user)

    bot_invitation =
      Factory.insert(:bot_invitation, user: user, invitee: user2, bot: bot)

    {:ok, user: user, user2: user2, bot: bot, bot_invitation: bot_invitation}
  end

  describe "create notification" do
    test "bot item", ctx do
      item = Factory.insert(:item, bot: ctx.bot, user: ctx.user2)

      assert {:ok, %Notification{} = notification} =
               Notification.notify(%BotItem{
                 user_id: ctx.user.id,
                 other_user_id: ctx.user2.id,
                 bot_id: ctx.bot.id,
                 bot_item_id: item.id
               })

      assert %Notification{type: :bot_item} =
               Repo.get_by(Notification, id: notification.id)
    end

    test "geofence event", ctx do
      assert {:ok, %Notification{} = notification} =
               Notification.notify(%GeofenceEvent{
                 user_id: ctx.user.id,
                 other_user_id: ctx.user2.id,
                 bot_id: ctx.bot.id,
                 event: :enter
               })

      assert %Notification{type: :geofence_event} =
               Repo.get_by(Notification, id: notification.id)
    end

    test "bot invitation", ctx do
      assert {:ok, %Notification{} = notification} =
               Notification.notify(%BotInvitation{
                 user_id: ctx.user.id,
                 other_user_id: ctx.user2.id,
                 bot_id: ctx.bot.id,
                 bot_invitation_id: ctx.bot_invitation.id
               })

      assert %Notification{type: :bot_invitation} =
               Repo.get_by(Notification, id: notification.id)
    end

    test "bot invitation response", ctx do
      assert {:ok, %Notification{} = notification} =
               Notification.notify(%BotInvitationResponse{
                 user_id: ctx.user.id,
                 other_user_id: ctx.user2.id,
                 bot_id: ctx.bot.id,
                 bot_invitation_id: ctx.bot_invitation.id,
                 bot_invitation_accepted: true
               })

      assert %Notification{type: :bot_invitation_response} =
               Repo.get_by(Notification, id: notification.id)
    end

    test "user invitation", ctx do
      assert {:ok, %Notification{} = notification} =
               Notification.notify(%UserInvitation{
                 user_id: ctx.user.id,
                 other_user_id: ctx.user2.id
               })

      assert %Notification{type: :user_invitation} =
               Repo.get_by(Notification, id: notification.id)
    end
  end

  describe "blocked user notification should fail" do
    setup ctx do
      Block.block(ctx.user, ctx.user2)
      :ok
    end

    test "bot item", ctx do
      item = Factory.insert(:item, bot: ctx.bot, user: ctx.user2)

      assert {:error, :invalid_user} ==
               Notification.notify(%BotItem{
                 user_id: ctx.user.id,
                 other_user_id: ctx.user2.id,
                 bot_id: ctx.bot.id,
                 bot_item_id: item.id
               })
    end

    test "geofence event", ctx do
      assert {:error, :invalid_user} ==
               Notification.notify(%GeofenceEvent{
                 user_id: ctx.user.id,
                 other_user_id: ctx.user2.id,
                 bot_id: ctx.bot.id,
                 event: :enter
               })
    end

    test "invitation", ctx do
      assert {:error, :invalid_user} ==
               Notification.notify(%BotInvitation{
                 user_id: ctx.user.id,
                 other_user_id: ctx.user2.id,
                 bot_id: ctx.bot.id,
                 bot_invitation_id: ctx.bot_invitation.id
               })
    end

    test "invitation response", ctx do
      assert {:error, :invalid_user} ==
               Notification.notify(%BotInvitationResponse{
                 user_id: ctx.user.id,
                 other_user_id: ctx.user2.id,
                 bot_id: ctx.bot.id,
                 bot_invitation_id: ctx.bot_invitation.id,
                 bot_invitation_accepted: true
               })
    end

    test "user invitation", ctx do
      assert {:error, :invalid_user} ==
               Notification.notify(%UserInvitation{
                 user_id: ctx.user.id,
                 other_user_id: ctx.user2.id
               })
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

  describe "decode notification" do
    test "bot item" do
      notification = Factory.build(:bot_item_notification)
      assert %BotItem{} = Notification.decode(notification)
    end

    test "geofence event" do
      notification = Factory.build(:geofence_event_notification)
      assert %GeofenceEvent{} = Notification.decode(notification)
    end

    test "invitation" do
      notification = Factory.build(:bot_invitation_notification)
      assert %BotInvitation{} = Notification.decode(notification)
    end

    test "invitation response" do
      notification = Factory.build(:bot_invitation_response_notification)
      assert %BotInvitationResponse{} = Notification.decode(notification)
    end

    test "user invitation" do
      notification = Factory.build(:user_invitation_notification)
      assert %UserInvitation{} = Notification.decode(notification)
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

    defp ids_match(a, b), do: Enum.map(a, & &1.id) == Enum.map(b, & &1.id)
  end
end
