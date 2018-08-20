defmodule Wocky.User.NotificationTest do
  use Wocky.DataCase

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
    test "bot item", shared do
      item = Factory.insert(:item, bot: shared.bot, user: shared.user2)

      assert {:ok, %Notification{} = notification} =
               Notification.notify(%BotItem{
                 user_id: shared.user.id,
                 other_user_id: shared.user2.id,
                 bot_id: shared.bot.id,
                 bot_item_id: item.id
               })

      assert %Notification{type: :bot_item} =
               Repo.get_by(Notification, id: notification.id)
    end

    test "geofence event", shared do
      assert {:ok, %Notification{} = notification} =
               Notification.notify(%GeofenceEvent{
                 user_id: shared.user.id,
                 other_user_id: shared.user2.id,
                 bot_id: shared.bot.id,
                 event: :enter
               })

      assert %Notification{type: :geofence_event} =
               Repo.get_by(Notification, id: notification.id)
    end

    test "invitation", shared do
      assert {:ok, %Notification{} = notification} =
               Notification.notify(%Invitation{
                 user_id: shared.user.id,
                 other_user_id: shared.user2.id,
                 bot_id: shared.bot.id,
                 invitation_id: shared.invitation.id
               })

      assert %Notification{type: :invitation} =
               Repo.get_by(Notification, id: notification.id)
    end

    test "invitation response", shared do
      assert {:ok, %Notification{} = notification} =
               Notification.notify(%InvitationResponse{
                 user_id: shared.user.id,
                 other_user_id: shared.user2.id,
                 bot_id: shared.bot.id,
                 invitation_id: shared.invitation.id,
                 accepted: true
               })

      assert %Notification{type: :invitation_response} =
               Repo.get_by(Notification, id: notification.id)
    end

    test "user follow", shared do
      assert {:ok, %Notification{} = notification} =
               Notification.notify(%UserFollow{
                 user_id: shared.user.id,
                 other_user_id: shared.user2.id
               })

      assert %Notification{type: :user_follow} =
               Repo.get_by(Notification, id: notification.id)
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
