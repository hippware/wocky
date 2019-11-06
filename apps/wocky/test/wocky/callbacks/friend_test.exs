defmodule Wocky.Callbacks.FriendTest do
  use Wocky.WatcherCase

  alias Wocky.Callbacks.Friend, as: Callback
  alias Wocky.Friends
  alias Wocky.Friends.Friend
  alias Wocky.Friends.Share.Cache
  alias Wocky.Notifier.InBand.Notification
  alias Wocky.Repo.Factory

  setup_all do
    Callback.register()
  end

  setup do
    [user, friend1, friend2] = Factory.insert_list(3, :user)

    Friends.befriend(user, friend1, notify: false)
    Friends.befriend(user, friend2, notify: false)

    {:ok, friendship1} = Friends.update_sharing(user, friend1, :always)
    {:ok, friendship2} = Friends.update_sharing(user, friend2, :disabled)

    {:ok,
     user: user,
     friend1: friend1,
     friend2: friend2,
     friendship1: friendship1,
     friendship2: friendship2}
  end

  describe "notifications" do
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

      Friends.update_sharing(ctx.user, ctx.friend1, :disabled)

      assert_eventually(in_band_notification_count(ctx.friend1) == 2)

      other_id = ctx.user.id

      assert [
               %Notification{
                 type: :location_share_end,
                 other_user_id: ^other_id
               }
             ] = ctx.friend1 |> Notification.user_query(nil, id) |> Repo.all()
    end

    test """
         ending a location share generates a notification for the sharing user
         """,
         ctx do
      Friends.update_sharing(ctx.user, ctx.friend1, :disabled)

      assert_eventually(in_band_notification_count(ctx.user) == 1)

      other_id = ctx.friend1.id

      assert [
               %Notification{
                 type: :location_share_end_self,
                 other_user_id: ^other_id
               }
             ] = ctx.user |> Notification.user_query(nil, nil) |> Repo.all()
    end
  end

  describe "location share cache" do
    test "starting a share inserts it into the cache", ctx do
      assert_eventually(
        [Friend.to_cached(ctx.friendship1)] == Cache.get(ctx.user.id)
      )
    end

    test "starting another share inserts it into the cache", ctx do
      {:ok, _} = Friends.update_sharing(ctx.user, ctx.friend2, :always)

      assert_eventually(
        [ctx.friendship1, %{ctx.friendship2 | share_type: :always}]
        |> Enum.map(&Friend.to_cached/1)
        |> Enum.sort() ==
          ctx.user.id |> Cache.get() |> Enum.sort()
      )
    end

    test "ending a share removes it from the cache", ctx do
      assert_eventually(
        [Friend.to_cached(ctx.friendship1)] == Cache.get(ctx.user.id)
      )

      Friends.stop_sharing_location(ctx.user)
      assert_eventually([] == Cache.get(ctx.user.id))
    end

    test "unfriending updates the cache", ctx do
      assert_eventually(
        [Friend.to_cached(ctx.friendship1)] == Cache.get(ctx.user.id)
      )

      Friends.unfriend(ctx.user, ctx.friend1)
      assert_eventually([] == Cache.get(ctx.user.id))
    end
  end

  defp in_band_notifications(user),
    do: user |> Notification.user_query(nil, nil) |> Repo.all()

  defp in_band_notification_count(user),
    do: user |> in_band_notifications() |> length()
end
