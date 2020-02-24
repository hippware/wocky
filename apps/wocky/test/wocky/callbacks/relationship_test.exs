defmodule Wocky.Callbacks.RelationshipTest do
  use Wocky.WatcherCase

  alias Faker.Code
  alias Pigeon.APNS.Notification, as: PushNotification
  alias Wocky.Callbacks.Relationship, as: Callback
  alias Wocky.Contacts
  alias Wocky.Contacts.Share.Cache
  alias Wocky.Contacts.Share.CachedRelationship
  alias Wocky.NotificationHelper
  alias Wocky.Notifier.InBand
  alias Wocky.Notifier.InBand.Notification
  alias Wocky.Notifier.Push
  alias Wocky.Notifier.Push.Backend.Sandbox
  alias Wocky.UserHelper

  setup_all do
    Callback.register()
  end

  setup do
    [user, friend1, friend2] = Factory.insert_list(3, :user)

    UserHelper.befriend(user, friend1)
    UserHelper.befriend(user, friend2)

    {:ok, friendship1} = Contacts.update_sharing(user, friend1, :always)
    {:ok, friendship2} = Contacts.update_sharing(user, friend2, :disabled)

    {:ok,
     user: user,
     friend1: friend1,
     friend2: friend2,
     friendship1: friendship1,
     friendship2: friendship2}
  end

  describe "location share notifications" do
    test "creating a location share generates a notification", ctx do
      assert_eventually(in_band_notification_count(ctx.friend1) == 1)
      other_id = ctx.user.id

      assert %Notification{
               type: :location_share,
               id: id,
               other_user_id: ^other_id
             } = Repo.get_by(Notification, user_id: ctx.friend1.id)
    end

    test """
         ending a location share generates a notification for the shared-to user
         """,
         ctx do
      assert_eventually(in_band_notification_count(ctx.friend1) == 1)
      %Notification{id: id} = Repo.get_by(Notification, user_id: ctx.friend1.id)

      Contacts.update_sharing(ctx.user, ctx.friend1, :disabled)

      assert_eventually(in_band_notification_count(ctx.friend1) == 2)

      other_id = ctx.user.id

      assert [
               %Notification{
                 type: :location_share_end,
                 other_user_id: ^other_id
               }
             ] = in_band_notifications(ctx.friend1, nil, id)
    end

    test """
         ending a location share generates a notification for the sharing user
         """,
         ctx do
      Contacts.update_sharing(ctx.user, ctx.friend1, :disabled)

      assert_eventually(in_band_notification_count(ctx.user) == 1)

      other_id = ctx.friend1.id

      assert [
               %Notification{
                 type: :location_share_end_self,
                 other_user_id: ^other_id
               }
             ] = in_band_notifications(ctx.user)
    end

    test """
         unfriending generates an end-share notification for the sharing user
         """,
         ctx do
      assert_eventually(in_band_notification_count(ctx.friend1) == 1)
      InBand.delete(ctx.friend1, ctx.user)

      Contacts.unfriend(ctx.user, ctx.friend1)

      other_id = ctx.user.id

      assert_eventually(in_band_notification_count(ctx.friend1) == 1)

      assert [
               %Notification{
                 type: :location_share_end,
                 other_user_id: ^other_id
               }
             ] = in_band_notifications(ctx.friend1)
    end
  end

  describe "location share cache" do
    test "starting a share inserts it into the cache", ctx do
      assert_eventually(
        [CachedRelationship.new(ctx.friendship1)] == Cache.get(ctx.user.id)
      )
    end

    test "starting another share inserts it into the cache", ctx do
      {:ok, _} = Contacts.update_sharing(ctx.user, ctx.friend2, :always)

      assert_eventually(
        [ctx.friendship1, %{ctx.friendship2 | share_type: :always}]
        |> Enum.map(&CachedRelationship.new/1)
        |> Enum.sort() ==
          ctx.user.id |> Cache.get() |> Enum.sort()
      )
    end

    test "ending a share removes it from the cache", ctx do
      assert_eventually(
        [CachedRelationship.new(ctx.friendship1)] == Cache.get(ctx.user.id)
      )

      Contacts.stop_sharing_location(ctx.user)
      assert_eventually([] == Cache.get(ctx.user.id))

      # Make sure the notifications have fired
      assert_eventually(in_band_notification_count(ctx.user) >= 1)
      assert_eventually(in_band_notification_count(ctx.friend1) >= 1)
    end

    test "unfriending updates the cache", ctx do
      assert_eventually(
        [CachedRelationship.new(ctx.friendship1)] == Cache.get(ctx.user.id)
      )

      Contacts.unfriend(ctx.user, ctx.friend1)
      assert_eventually([] == Cache.get(ctx.user.id))

      # Make sure the notifications have fired
      assert_eventually(in_band_notification_count(ctx.user) >= 1)
      assert_eventually(in_band_notification_count(ctx.friend1) >= 1)
    end
  end

  describe "befriend notifications" do
    setup do
      user1 = Factory.insert(:user, device: Factory.device())
      user2 = Factory.insert(:user, device: Factory.device())

      {:ok, user1: user1, user2: user2}
    end

    test "befriending generates notification",
         ctx = %{user1: %{id: user1_id}, user2: %{id: user2_id}} do
      Contacts.befriend(ctx.user1, ctx.user2, :always)

      assert_eventually(in_band_notification_count(ctx.user1) == 2)

      notifications! = in_band_notifications(ctx.user1)

      assert Enum.any?(
               notifications!,
               &match?(
                 %Notification{
                   type: :user_befriend,
                   other_user_id: ^user2_id
                 },
                 &1
               )
             )

      assert Enum.any?(
               notifications!,
               &match?(
                 %Notification{
                   type: :location_share,
                   other_user_id: ^user2_id
                 },
                 &1
               )
             )

      assert_eventually(in_band_notification_count(ctx.user2) == 2)

      notifications! = in_band_notifications(ctx.user2)

      assert Enum.any?(
               notifications!,
               &match?(
                 %Notification{
                   type: :user_befriend,
                   other_user_id: ^user1_id
                 },
                 &1
               )
             )

      assert Enum.any?(
               notifications!,
               &match?(
                 %Notification{
                   type: :location_share,
                   other_user_id: ^user1_id
                 },
                 &1
               )
             )
    end

    test "befriend push notifications", ctx do
      token = Code.isbn13()
      :ok = Push.enable(ctx.user1, ctx.user1.device, token)
      Sandbox.clear_notifications(global: true)

      Contacts.make_friends(ctx.user1, ctx.user2, :always)
      Contacts.make_friends(ctx.user2, ctx.user1, :always)

      # Look for two, but should receive only 1 - user1 shouldn't get a
      # location share notification because we want to avoid spamming pushes
      # on friendship
      [n] = Sandbox.wait_notifications(count: 2, timeout: 100, global: true)

      assert %PushNotification{payload: %{"aps" => %{"alert" => message}}} = n

      assert message = "@#{ctx.user2.handle} connected with you"
    end

    test "accepting invitation generates notification",
         ctx = %{user1: %{id: user1_id}, user2: %{id: user2_id}} do
      Contacts.make_friends(ctx.user1, ctx.user2, :disabled)

      # Clear invitation notification
      NotificationHelper.clear_expected_notifications(ctx.user2, 1)

      assert in_band_notification_count(ctx.user1) == 0

      Contacts.make_friends(ctx.user2, ctx.user1, :disabled)

      assert_eventually(in_band_notification_count(ctx.user1) == 1)

      assert [
               %Notification{
                 type: :user_befriend,
                 other_user_id: ^user2_id
               }
             ] = in_band_notifications(ctx.user1)

      assert_eventually(in_band_notification_count(ctx.user2) == 1)

      assert [
               %Notification{
                 type: :user_befriend,
                 other_user_id: ^user1_id
               }
             ] = in_band_notifications(ctx.user2)
    end
  end

  describe "location_share when not friends" do
    setup do
      [user, friend] = Factory.insert_list(2, :user)
      {:ok, user: user, friend: friend}
    end

    test "should not send location share on initial invitation", ctx do
      Contacts.make_friends(ctx.user, ctx.friend, :always)

      # Should only generate the invitation notification
      assert_eventually(in_band_notification_count(ctx.friend) == 1)
      assert in_band_notification_count(ctx.user) == 0
    end
  end

  defp in_band_notifications(user, before_id \\ nil, after_id \\ nil),
    do: user |> InBand.user_query(before_id, after_id) |> Repo.all()

  defp in_band_notification_count(user),
    do: user |> in_band_notifications() |> length()
end
