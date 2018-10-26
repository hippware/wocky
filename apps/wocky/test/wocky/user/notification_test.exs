defmodule Wocky.User.NotificationTest do
  use Wocky.DataCase

  alias Wocky.Block
  alias Wocky.Repo.Factory
  alias Wocky.User.Notification

  alias Wocky.User.Notification.{
    BotItem,
    GeofenceEvent,
    Invitation,
    InvitationResponse,
    UserFollow
  }

  setup do
    [user, user2] = Factory.insert_list(2, :user)
    bot = Factory.insert(:bot, user: user)

    invitation =
      Factory.insert(:invitation, user: user, invitee: user2, bot: bot)

    {:ok, user: user, user2: user2, bot: bot, invitation: invitation}
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

    test "invitation", ctx do
      assert {:ok, %Notification{} = notification} =
               Notification.notify(%Invitation{
                 user_id: ctx.user.id,
                 other_user_id: ctx.user2.id,
                 bot_id: ctx.bot.id,
                 invitation_id: ctx.invitation.id
               })

      assert %Notification{type: :invitation} =
               Repo.get_by(Notification, id: notification.id)
    end

    test "invitation response", ctx do
      assert {:ok, %Notification{} = notification} =
               Notification.notify(%InvitationResponse{
                 user_id: ctx.user.id,
                 other_user_id: ctx.user2.id,
                 bot_id: ctx.bot.id,
                 invitation_id: ctx.invitation.id,
                 accepted: true
               })

      assert %Notification{type: :invitation_response} =
               Repo.get_by(Notification, id: notification.id)
    end

    test "user follow", ctx do
      assert {:ok, %Notification{} = notification} =
               Notification.notify(%UserFollow{
                 user_id: ctx.user.id,
                 other_user_id: ctx.user2.id
               })

      assert %Notification{type: :user_follow} =
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
               Notification.notify(%Invitation{
                 user_id: ctx.user.id,
                 other_user_id: ctx.user2.id,
                 bot_id: ctx.bot.id,
                 invitation_id: ctx.invitation.id
               })
    end

    test "invitation response", ctx do
      assert {:error, :invalid_user} ==
               Notification.notify(%InvitationResponse{
                 user_id: ctx.user.id,
                 other_user_id: ctx.user2.id,
                 bot_id: ctx.bot.id,
                 invitation_id: ctx.invitation.id,
                 accepted: true
               })
    end

    test "user follow", ctx do
      assert {:error, :invalid_user} ==
               Notification.notify(%UserFollow{
                 user_id: ctx.user.id,
                 other_user_id: ctx.user2.id
               })
    end
  end

  describe "delete/2" do
    setup %{user: user, user2: user2} do
      user3 = Factory.insert(:user)

      notification =
        Factory.insert(:invitation_notification, user: user, other_user: user2)

      notification2 =
        Factory.insert(:invitation_notification, user: user, other_user: user3)

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
      notification = Factory.build(:invitation_notification)
      assert %Invitation{} = Notification.decode(notification)
    end

    test "invitation response" do
      notification = Factory.build(:invitation_response_notification)
      assert %InvitationResponse{} = Notification.decode(notification)
    end

    test "user follow" do
      notification = Factory.build(:user_follow_notification)
      assert %UserFollow{} = Notification.decode(notification)
    end
  end
end
